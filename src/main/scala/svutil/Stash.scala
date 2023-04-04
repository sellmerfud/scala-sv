

package svutil

import scala.util.matching.Regex
import java.io.{ File, FileWriter, PrintWriter,  FileReader, BufferedReader }
import java.nio.file.{ Files, Paths, Path }
import java.util.regex.PatternSyntaxException
import java.time.LocalDateTime
import java.util.UUID
import scala.jdk.CollectionConverters._
import com.typesafe.config._
import scala.xml._
import org.sellmerfud.optparse._
import Exec.runCmd
import Color._
import Utilities._

object Stash extends Command {
  
  override val name = "stash"
  override val description = "Stash away changes to a dirty working copy"

  private sealed trait StashCommand {
    val cmdName: String
    def run(args: Seq[String]): Unit
  }

  // Values for the items in svn status
  private val UNVERSIONED = "unversioned"
  private val NORMAL      = "normal"
  private val ADDED       = "added"
  private val DELETED     = "deleted"
  private val MODIFIED    = "modified"
  
  // path for directories wil end with a slash/  (only happens for remved directories)
  // revision of the file when it was modified (for display only)
  // status will be one of: deleted, modified, added, unversioned
  case class StashItem(path: String, revision: String, status: String, isDir: Boolean)
  
  case class StashEntry(
    branch: String,        // branch from which the stash was created (trunk, 8.1, etc.)
    revision: String,      // commit revision of working copy root when stash was created
    description: String,
    date: LocalDateTime,   // time of creation
    patchName: String,     // name of patch file for this stash relative to .sv/stash/
    items: Seq[StashItem])

  def stashPath         = getDataDirectory().resolve("stash")
  def stashEntriesFile  = stashPath.resolve("stash_entries.json").toFile
  def createPatchName() = s"${UUID.randomUUID}.patch"

  private def toConfigObject(stashEntries: Seq[StashEntry]): ConfigObject = {

    def itemToConfig(item: StashItem): ConfigObject =
      ConfigValueFactory.fromMap(Map(
        "path" -> item.path,
        "revision" -> item.revision,
        "status"   -> item.status,
        "isDir"    -> item.isDir
      ).asJava)
    
    
    def entryToConfig(entry: StashEntry): ConfigObject =
      ConfigValueFactory.fromMap(Map(
        "branch"      -> entry.branch,
        "revision"    -> entry.revision,
        "description" -> entry.description,
        "date"        -> toISODateString(entry.date),
        "patchName"   -> entry.patchName,
        "items"       -> ConfigValueFactory.fromIterable(entry.items.map(itemToConfig).asJava)
      ).asJava)
      
    ConfigValueFactory.fromMap(Map(
      "entries" -> ConfigValueFactory.fromIterable(stashEntries.map(entryToConfig).asJava)
    ).asJava)
  }
  
  private def fromConfig(cfg: Config): List[StashEntry] = {
    
    def toStashItem(cfg: Config): StashItem = 
      StashItem(
        cfg.getString("path"),
        cfg.getString("revision"),
        cfg.getString("status"),
        cfg.getBoolean("isDir")
      )
      
    def toStashEntry(cfg: Config): StashEntry =
      StashEntry(
        cfg.getString("branch"),
        cfg.getString("revision"),
        cfg.getString("description"),
        parseISODate(cfg.getString("date")),
        cfg.getString("patchName"),
        cfg.getConfigList("items").asScala.toSeq map toStashItem
      )
  
      cfg.getConfigList("entries").asScala.toList map toStashEntry
  }

  // .sv/stash
  // .sv/stash/entries.json
  // .sv/stash/entryname.diff...
  private def loadStashEntries(): List[StashEntry] = {
    val file = stashEntriesFile
    
    if (file.isFile && file.canRead) {
      try {
        val config = ConfigFactory.parseFile(file, ConfigParseOptions.defaults.setSyntax(ConfigSyntax.JSON))
        fromConfig(config)
      }
      catch {
        case e: Throwable => 
          generalError(s"Error reading stash entries ($file): ${e.getMessage}")
      }
    }
    else if (file.exists) {
      generalError(s"Unable to read stash entries ($file): Check file permissions")
    }
    else
      Nil
  }
   
  def saveStashEntries(entries: List[StashEntry]): Unit = {
    val file = stashEntriesFile
    try {
      val opts   = ConfigRenderOptions.concise.setJson(true).setFormatted(true)
      val writer = new FileWriter(file)
      writer.write(toConfigObject(entries).toConfig.root.render(opts))
      writer.close
    } 
    catch {
      case e: Throwable =>
        generalError(s"Error saving stash entries ($file): ${e.getMessage}")
    }
  }  
   

  // == Start Command ====================================================
  private case object Push extends StashCommand {
    override val cmdName = "push"
      
    // sv stash [push] [-u|--include-unversioned] [-d|--description=<desc>]
    private case class Options(
      unversioned: Boolean        = false,
      description: Option[String] = None)
        
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name [$cmdName]"
        
        banner = s"usage: $cmdPrefix [<options]"

        flag("-u", "--unversioned", "Include unversioned files in the stash")
          { _.copy(unversioned = true) }
        
        reqd[String]("-m", "--message=<msg>",    "A short description of the stash")
          { (desc, options) => options.copy(description = Some(desc)) }
        
        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
      }

      parser.parse(args, Options())
    }
    
    
    private def createPatchFile(wcRoot: Path, patchName: String): Unit = {
      val cmdLine = Seq("svn", "diff", "--depth=infinity", "--ignore-properties", ".")
      val diffOut = runCmd(cmdLine, Some(wcRoot.toFile))
      val file    = stashPath.resolve(patchName).toFile
      
      try {
        val writer = new PrintWriter(new FileWriter(file), true)
        diffOut foreach writer.println
        writer.close()
      }
      catch {
        case e: Throwable =>
          generalError(s"Error writing patch file ($file): ${e.getMessage}")
      }
    }
    
    private def getLogMessage1st(wcRoot: Path): String = {
      val cmdLine = Seq("svn", "log", "--xml", "--revision=HEAD", wcRoot.toString)
      val out     = runCmd(cmdLine)
      val entries = (XML.loadString(out.mkString("\n")) \ "logentry") map parseLogEntry
      
      entries.head.msg.headOption getOrElse ""
    }

    //  Runs `svn status` on the working copy root directory
    //  If we are not including unversioned items then we filter them out and build the list
    //
    //  If we are including unversioned items then it is a bit more complicated:
    //  `svn status` will include unversioned directories but will not include their contents
    //  So in this case we must add these unversioned directories to the working copy and then
    //  run `svn status` a second time.  The add operation is recursive so we only need to
    //  do it on the top level unversioned directories.
    //  At this point `svn status` will return all of the previously unversioned items as
    //  "added" so we must mark them as unversioned in our own item list.
    //  So this function will alter the working copy when unversioned items are being stashed.
    private def getStashItems(wcRoot: Path, unversioned: Boolean): List[StashItem] = {
      
      //  We always filter out entries with item status of "normal".  These have only property changes.
      //  which we do not care about.
      //  We also filter out entries with item status of "unversioned" unless the user choseen to
      //  include the unversioned files in the stash.
      val included    = (e: StatusEntry) =>
        e.itemStatus != NORMAL && (e.itemStatus != UNVERSIONED || unversioned)
        
      val toItem = (entry: StatusEntry) => {
        val isDir = wcRoot.resolve(entry.path).toFile.isDirectory
        StashItem(entry.path, entry.revision, entry.itemStatus, isDir)
      }
      
      // Get the status starting at the wcRoot to determine what is dirty and must be stashed
      def getWorkingCopyItems(): List[StashItem] = {
        getSvnStatus(".", Some(wcRoot.toFile)).entries.toList filter included map toItem
      }
      
      def addToWorkingCopy(paths: Seq[String]): Unit = {
        val cmdLine = Seq("svn", "add", "--depth=infinity", "--no-auto-props") ++ paths
        runCmd(cmdLine, Some(wcRoot.toFile))        
      }
      
      
      def fixupUnversionedItems(initialItems: List[StashItem]): List[StashItem] = {
        val unversionedPaths = (initialItems filter (_.status == UNVERSIONED) map (_.path))
        
        if (unversionedPaths.isEmpty)
          initialItems
        else {
          addToWorkingCopy(unversionedPaths)  // This recursively adds children of unversioned directories
          //  If there are no unversioned directories then no need to get the status again
          if (initialItems exists (item => item.isDir && item.status == UNVERSIONED)) {
            // Now we have unversioned items that have been added to the working-copy.
            // This time when we get the working copy status they will be reported as "added"
            // We need to detect them and set the status to "unversioned" for our item list.
            // These unversioned items will be those with paths that are equal to or start with
            // the `unversionedPaths` that we set earilier.
            
            def isUnversioned(item: StashItem) = item.status == ADDED &&
                                                 (unversionedPaths exists (prefix => item.path startsWith prefix))
            getWorkingCopyItems() map {
              case item if isUnversioned(item) => item.copy(status = UNVERSIONED)
              case item                        => item
            }
          }
          else
            initialItems
        }
      }
      
      
      // Start point of getStashItems()
      getWorkingCopyItems() match {
        case Nil                  => Nil
        case items if unversioned => fixupUnversionedItems(items)
        case items                => items
      }
    }
    
    
    override def run(args: Seq[String]): Unit = {
      val options = processCommandLine(args)
      val wcInfo  = getWorkingCopyInfo()  // Make sure the current directory is in a subversion working copy
      val wcRoot  = getWorkingCopyRoot().get  // All commands will be run from the top dir

      // Get the status starting at the wcRoot to determine what is dirty and must be stashed
      val wcStatus           = getSvnStatus(".", Some(wcRoot.toFile))
      
      val items = getStashItems(wcRoot, options.unversioned)
      if (items.isEmpty)
        successExit("No local changes to save")
      
      val (branch, revision) = getCurrentBranch(wcRoot.toString)
      val description        = options.description getOrElse getLogMessage1st(wcRoot)
      
      val patchName = createPatchName()
      stashPath.toFile.mkdir()  // Make sure stash directory exists
      createPatchFile(wcRoot, patchName)
      
      val stashEntry = StashEntry(
        branch,
        revision,
        description,
        LocalDateTime.now,
        patchName,
        items)
      
      // Put the new entry at the head of the list and save
      saveStashEntries(stashEntry :: loadStashEntries())
      
      // Lastly we revert the working copy.
      // We will explicitly revert all entries to ensure that the --remove-added flag is honored.
      // For added/unversioned directories we do not need to revert any entries below them
      // as these entrie will be reverted recursively with their respective  directories.
      val addedAndUnversionedDirs = items filter (i => i.isDir && (i.status == ADDED || i.status == UNVERSIONED))
      val canSkip = (item: StashItem) => addedAndUnversionedDirs exists (d => item.path.startsWith(d.path) && item.path != d.path)
      val revertPaths = items filterNot canSkip map (_.path)
      val cmdLine = Seq("svn", "revert", "--remove-added", "--depth=infinity") ++ revertPaths
      runCmd(cmdLine, Some(wcRoot.toFile))
        
      //  Let the user know we finished successfully
      println(s"Saved working copy state on ${green(branch)}[${yellow(revision)}]: $description")
    }
  }
  
  
  private case object List extends StashCommand {
    override val cmdName = "list"
      
    private def processCommandLine(args: Seq[String]): Unit = {

      val parser = new OptionParser[Unit] {
        val cmdPrefix = s"$scriptName $name $cmdName"
        
        banner = s"usage: $cmdPrefix [<options]"

        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
      }

      parser.parse(args, ())
    }
    
    override def run(args: Seq[String]): Unit = {
      processCommandLine(args) // User may have specified --help, -h
      getWorkingCopyInfo()     // Make sure the current directory is in a subversion working copy
      
      for ((stash, index) <- loadStashEntries().view.zipWithIndex) {
      
        println(s"stash-$index, on ${green(stash.branch)}[${yellow(stash.revision)}]: ${stash.description}")  
      }
    }
  }
     
  private def showHelp(): Nothing = {
    val sv = scriptName
    val help = s"""|Available stash commands:
                   |$sv $name push        Push dirty working changes copy to the stash
                   |                      and revert the working copy
                   |$sv $name list        Display stash list
                   |$sv $name show        Show the details of a stash entry
                   |$sv $name pop         Remove a stash entry and apply it to the working copy
                   |$sv $name apply       Apply a stash entry to the working copy
                   |$sv $name drop        Remove a stash entry
                   |$sv $name clear       Remove all stash entries
                   |
                   |Type '$sv $name <command> --help' for details on a specific command""".stripMargin
      println(help)
      throw HelpException()
  }

  private val stashCommands = Push::List::Nil

  private def matchCommand(cmdName: String, cmdList: List[StashCommand]): List[StashCommand] = {
    if ("""^[a-zA-Z][-a-zA-Z0-9_]*""".r matches cmdName)
      cmdList filter (_.cmdName startsWith cmdName)
    else
      Nil
  }

  private def getStashCommand(cmdName: String): StashCommand = {
    matchCommand(cmdName, stashCommands) match {
      case Nil        => showHelp()
      case cmd :: Nil => cmd
      case names      => generalError(s"$scriptName $name command '$cmdName' is ambiguous.  (${names.map(_.cmdName).mkString(", ")})")
    }
  }

  // Main entry point to bisect commnad
  override def run(args: Seq[String]): Unit = {

    if (args.nonEmpty && (args.head == "help" || args.head == "--help"))
      showHelp();
    else if (args.isEmpty || (args.head.startsWith("-") && args.head != "--"))
      Push.run(args)
    else
      getStashCommand(args.head).run(args.tail)
  }
}
