

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
  
  // path for directories wil end with a slash/  (only happens for remved directories)
  // revision of the file when it was modified (for display only)
  // status will be one of: deleted, modified, added, unversioned
  case class StashItem(path: String, revision: String, status: String)
  
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

    def itemToConfig(path: StashItem): ConfigObject =
      ConfigValueFactory.fromMap(
        Map("path" -> path.path,"revision" -> path.revision,"status" -> path.status).asJava)
    
    
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
      StashItem(cfg.getString("path"), cfg.getString("revision"), cfg.getString("status"))
      
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
      includeUnversioned: Boolean        = false,
      description:        Option[String] = None)
        
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name [$cmdName]"
        
        banner = s"usage: $cmdPrefix [<options]"

        flag("-u", "--include-unversioned", "Included unversioned files in the stash")
          { _.copy(includeUnversioned = true) }
        
        reqd[String]("-d", "--description=<desc>",    "A short description of the stash")
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

    override def run(args: Seq[String]): Unit = {
      val options = processCommandLine(args)
      val wcInfo  = getWorkingCopyInfo()  // Make sure the current directory is in a subversion working copy
      val wcRoot  = getWorkingCopyRoot().get  // All commands will be run from the top dir

      // Get the status starting at the wcRoot to determine what is dirty and must be stashed
      val wcStatus           = getSvnStatus(".", Some(wcRoot.toFile))
      val (branch, revision) = getCurrentBranch(wcRoot.toString)
      val description        = options.description getOrElse getLogMessage1st(wcRoot)
      
      //  Filter out entries with item status of "normal".  These have only property changes.
      //  Also filter out entries with item status of "unversioned" unless the user chose to
      //  stash unversioned files.
      val toStashItem = (entry: StatusEntry) => StashItem(entry.path, entry.revision, entry.itemStatus)
      val unwanted    = (entry: StatusEntry) =>
        entry.itemStatus  == "normal" ||
        (entry.itemStatus == "unversioned" && !options.includeUnversioned)
      val items     = wcStatus.entries filterNot unwanted map toStashItem
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
      
      val stashList = loadStashEntries()
      // Put out new entry at the head of the list and save
      saveStashEntries(stashEntry :: stashList)
      
      //  Show the user what happened
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
