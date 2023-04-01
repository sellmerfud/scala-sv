

package svutil

import java.io.{ File, FileWriter, PrintWriter,  FileReader, BufferedReader }
import java.nio.file.{ Files, Paths, Path }
import java.time._
import scala.xml._
import scala.jdk.CollectionConverters._
import scala.xml._
import scala.util.{ Try, Success, Failure }
import com.typesafe.config._
import org.sellmerfud.optparse._
import Exec._
import Color._
import Utilities._

object Bisect extends Command {
  
  override val name = "bisect"
  override val description = "Use binary search to find the commit that introduced a bug"

  // Ordering for a sequence of revisions
  // We sort them from High (most recent) to Low(least recent)
  val RevisionOrdering: Ordering[String] = Ordering.by { revision => -revision.toInt }

  case class ExtantEntry(revision: String, msg1st: String)
  
  private case class BisectData(
    localPath:   String,
    originalRev: String,                        // original working copy revision. Used to reset working copy
    startMaxRev: Option[String]   = None,       // Initial starting max, used to get extant revs
    startMinRev: Option[String]   = None,       // Initial starting min, used to get extant revs
    maxRev:      Option[String]   = None,       // maximum revision number in our list that is still being checked
    minRev:      Option[String]   = None,       // minimum revision number in our list that is still being checked
    extantRevs:  Seq[ExtantEntry] = Seq.empty,  // revisons between the original bad and good (inclusive)
    skipped:     Set[String]      = Set.empty,  // revisions that have been explicitly skipped
    termBad:     Option[String]   = None,
    termGood:    Option[String]   = None,
  ) {
    val termBadName  = termBad  getOrElse Bad.cmdName
    val termGoodName = termGood getOrElse Good.cmdName
    
    val isReady = startMaxRev.nonEmpty && startMinRev.nonEmpty
    
    //  List of revisions between maxRev and minRev (exclusive)
    def getCandidateRevisions() = {
      if (!isReady)
        throw new IllegalStateException("getCandidateRevisions() called when data not ready")
      val lowIndex = extantRevs indexWhere (_.revision == maxRev.get)
      val hiIndex  = extantRevs indexWhere (_.revision == minRev.get)
      extantRevs.slice(lowIndex + 1, hiIndex)
    }
    
    def getExtant(revision: String) = extantRevs find (_.revision == revision)
    def isExtant(revision: String)  = extantRevs exists (_.revision == revision)
    def getLogMsg(revision: String) = extantRevs find (_.revision == revision) map (_.msg1st) getOrElse ""
  }
  
  //  Try to limit the revs to only those in the cadidate list
  //  If the data is not ready then just return the whole set
  def candidatesOnly(data: BisectData, revisions: Set[String]): Set[String] = {
    if (data.isReady) {
      val candidates = (data.getCandidateRevisions() map (_.revision)).toSet
      revisions filter candidates.contains
    }
    else
      revisions
  }
  
  
  implicit class ConfigWrapper(cfg: Config) {
    def optString(path: String): Option[String] = if (cfg.getIsNull(path)) None else Some(cfg.getString(path))
  }
    
  //  Convert our BisectData to a Config value that can be saved to disk.
  private def toConfigObject(data: BisectData): ConfigObject = {
    
    def toExtantConfig(entry: ExtantEntry): ConfigValue = {
      ConfigValueFactory.fromMap(Map("revision" -> entry.revision, "msg1st" -> entry.msg1st).asJava)
    }
    
    ConfigValueFactory.fromMap(Map(
      "localPath"   -> data.localPath,
      "originalRev" -> data.originalRev,
      "startMaxRev" -> data.startMaxRev.getOrElse(null),
      "startMinRev" -> data.startMinRev.getOrElse(null),
      "maxRev"      -> data.maxRev.getOrElse(null),
      "minRev"      -> data.minRev.getOrElse(null),
      "extantRevs"  -> ConfigValueFactory.fromIterable(data.extantRevs.map(toExtantConfig).asJava),
      "skipped"     -> ConfigValueFactory.fromIterable(data.skipped.asJava),
      "termBad"     -> data.termBad.getOrElse(null),
      "termGood"    -> data.termGood.getOrElse(null),
    ).asJava)
  }
  
  private def fromConfig(cfg: Config) = {
    def toExtantEntry(cfg: Config): ExtantEntry = {
      ExtantEntry(cfg.getString("revision"), cfg.getString("msg1st"))
    }
    
    BisectData(
      cfg.getString("localPath"),
      cfg.getString("originalRev"),
      cfg.optString("startMaxRev"),
      cfg.optString("startMinRev"),
      cfg.optString("maxRev"),
      cfg.optString("minRev"),
      cfg.getConfigList("extantRevs").asScala.toSeq map toExtantEntry,
      cfg.getStringList("skipped").asScala.toSet,
      cfg.optString("termBad"),
      cfg.optString("termGood"),
    )
  }

  private def findSvnDir(): File = {
    def findIt(directory: Path): File = {
      if (directory == null)
        generalError(s"You must run this command from within a subversion working copy directory")
      else
        directory.resolve(".svn").toFile match {
          case svn if svn.isDirectory => svn
          case _                      => findIt(directory.getParent) 
        }
    }
    
    // Start with the current working directory
    findIt(Paths.get("").toAbsolutePath)
  }
  
  
  private def findSvnTmp(): File = {

      def findIt(directory: Path): File = {
        if (directory == null)
          generalError(s"Your current working directory is not with a subversion working copy")
        else if (directory.resolve(".svn").toFile.isDirectory) {
          // Found the .svn directory
          val tmpDir = directory.resolve(".svn/tmp").toFile
          if (tmpDir.isDirectory) {
              if (tmpDir.canRead && tmpDir.canWrite)
                tmpDir
              else
                generalError(s"Error, insufficent file permissions for ${tmpDir}")
          }
          else if (tmpDir.isFile)
            generalError(s"Error, ${tmpDir} is not a directory")
          else if (tmpDir.mkdir())
            tmpDir
          else
            generalError(s"Error, ${tmpDir} does not exist and cannot be created")
        }
        else
          findIt(directory.getParent)
      }
      
      // Start with the current working directory
      findIt(Paths.get("").toAbsolutePath)
  }
  
  //  Verifiy that the currrent working directory is an SVN working copy
  //  and that we are at the top of that working copy.
  private def getWorkingCopyInfo(): SvnInfo = {
    try getSvnInfo(".")
    catch {
      case ExecError(_, _) =>
        generalError(s"$scriptName $name must be run from within a subversion working copy directory.")
        
      case e: Throwable =>
        generalError("Error verifying working copy\n" + (Option(e.getMessage) getOrElse e.getClass.getName))
    }
  }
    
  private def bisectDataFile = new File(findSvnDir(), "tmp/sv_bisect_data.json")
  private def bisectLogFile  = new File(findSvnDir(), "tmp/sv_bisect_log")
    
    
  private def loadBisectData(): Option[BisectData] = {
    val dataFile = bisectDataFile
    
    if (dataFile.isFile && dataFile.canRead) {
      try {
        val config = ConfigFactory.parseFile(dataFile, ConfigParseOptions.defaults.setSyntax(ConfigSyntax.JSON))
        val data   = fromConfig(config)
        val cwd    = Paths.get("").toAbsolutePath
        if (cwd.toString != data.localPath)
          generalError(s"$scriptName $name must be run from the same directory where the bisect session was started: ${data.localPath}")
        Some(data)
      }
      catch {
        case e: Throwable => 
          generalError(s"Error reading bisect data ($dataFile): ${e.getMessage}")
      }
    }
    else if (dataFile.isFile) {
      generalError(s"Unable to read bisect data ($dataFile): Check file permissions")
    }
    else
      None
  }
  
  def saveBisectData(data: BisectData): Unit = {
    val dataFile = bisectDataFile
    try {
      val opts   = ConfigRenderOptions.concise.setJson(true).setFormatted(true)
      val writer = new FileWriter(dataFile)
      writer.write(toConfigObject(data).toConfig.root.render(opts))
      writer.close
    } 
    catch {
      case e: Throwable =>
        generalError(s"Error saving bisect data ($dataFile): ${e.getMessage}")
    }
  }  
  
  
  //  Load and return the bisect data or throw a general error
  //  if the data file is missing.
  private def getBisectData(): BisectData = {
    loadBisectData() getOrElse {
      generalError(s"You must first start a bisect session with '$scriptName $name ${Start.cmdName}'")
    }
  }
  
  private def appendToBisectLog(msg: String): Unit = {
    try {
      val writer = new PrintWriter(new FileWriter(bisectLogFile, true), true)
      writer.println(msg)
      writer.close()
    }
    catch {
      case e: Throwable =>
        generalError(s"Error appending to bisect log ($bisectLogFile): ${e.getMessage}")
    }
  }
  
  private def displayBisectLog(): Unit = {
    def read1Line(reader: BufferedReader): Unit =
        reader.readLine match {
          case null => // Reached eof
          case line =>
            System.out.println(line)
            read1Line(reader)
        }
        
    try {
      val reader = new BufferedReader(new FileReader(bisectLogFile))
      read1Line(reader)
      reader.close()
    }
    catch {
      case e: Throwable =>
        generalError(s"Error reading bisect log ($bisectLogFile): ${e.getMessage}")
    }
  }
  
  private def logBisectRevision(revision: String, term: String, msg1st: String): Unit = {
    appendToBisectLog(s"# $term: [$revision] $msg1st")
  }
  
  // The cmdLine should start with the biscect sub command
  private def logBisectCommand(cmdLine: Seq[String]): Unit = {
    appendToBisectLog((scriptPath +: name +: cmdLine).mkString(" "))
  }
  
  // The cmdLine should start with the biscect sub command
  private def displayBisectCommand(cmdLine: Seq[String]): Unit = {
    println((scriptName +: name +: cmdLine).mkString(" "))
  }

  //  We return the actual maxRev and minRev that exist in the history for
  //  the requested range
  private def getExtantRevisions(rev1: String, rev2: String): (String, String, Seq[ExtantEntry]) = {
    println(s"Fetching history from ${yellow(rev1)} to ${yellow(rev2)}")
    val logXML = XML.loadString(runCmd(Seq("svn", "log", "--xml", "--stop-on-copy", s"--revision=$rev1:$rev2", ".")).mkString("\n"))
    val extants = (logXML \ "logentry") map { node =>
      val entry = parseLogEntry(node)
      ExtantEntry(entry.revision, entry.msg.headOption getOrElse "")
    }
    
    extants.size match {
      case 0 => generalError(s"There is no working copy history in range $rev1 to $rev2")
      case 1 => generalError(s"There is only one commit in the working copy history range $rev1 to $rev2")
      case _ => (extants.head.revision, extants.last.revision, extants)
    }
  }
  
  private def getWaitingStatus(data: BisectData): Option[String] = {
    val bad  = data.termBadName
    val good = data.termGoodName
    (data.maxRev, data.minRev) match {
      case (None, None)    => Some(s"status: waiting for both '$good' and '$bad' revisions")
      case (Some(_), None) => Some(s"status: waiting for a '$good' revision")
      case (None, Some(_)) => Some(s"status: waiting for a '$bad' revision")
      case _               => None
    }
  }
  
  private def getLogEntry(revision: String, withPaths: Boolean = false): Option[LogEntry] = {
    val verbose  = if (withPaths) Seq("--verbose") else Seq.empty
    val cmdLine  = Seq("svn", "log", "--xml", s"--revision=$revision", "--limit=1", ".") ++ verbose
    val logXML   = XML.loadString(runCmd(cmdLine).mkString("\n"))
    (logXML \ "logentry").headOption map parseLogEntry
  }
  
  // Return true if the bisect is complete
  private def performBisect(data: BisectData): Boolean = {
    if (!data.isReady)
      throw new IllegalStateException("performBisect() called when data not ready")

    val maxRev = data.maxRev.get
    val minRev = data.minRev.get
    val candidateRevs  = data.getCandidateRevisions()
    val nonSkippedRevs = candidateRevs filterNot (r => data.skipped.contains(r.revision))
    
    if (nonSkippedRevs.isEmpty) {
      if (candidateRevs.nonEmpty) {
          val maxEntry = data.getExtant(maxRev).get
          println("\nThere are only skipped revisions left to test.")
          println(s"The first '${data.termBadName}' commit could be any of:")
          for (entry <- (maxEntry +: candidateRevs))
            s"${println(yellow(entry.revision))} ${entry.msg1st}"
          println("We cannot bisect more!")
          true
      }
      else {
        println(s"\nThe first '${data.termBadName}' commit is: ${yellow(maxRev)}")
        getLogEntry(maxRev, withPaths = true) foreach { log => showCommit(log) }
        true
      }
    }
    else {
      import scala.math.log10
      val num     = nonSkippedRevs.size
      val steps   = (log10(num) / log10(2)).toInt match {
        case 1 => "1 step"
        case n => s"$n steps"
      }
      val nextRev = nonSkippedRevs(nonSkippedRevs.size / 2)
      
      println(s"Bisecting: $num revisions left to test after this (roughly $steps)")
      updateWorkingCopy(nextRev.revision, Some(data))
      false
    }
  }
  
  private def get1stLogMessage(revision: String, data: Option[BisectData]): String = {
    data match {
      case Some(d) if d.isReady && d.isExtant(revision) =>
        d.getLogMsg(revision)
        
      case _ =>
        getLogEntry(revision) flatMap (_.msg.headOption) getOrElse ""
    }
  }
  
  private def updateWorkingCopy(revision: String, data: Option[BisectData] = None): Unit = {
    val msg1st = get1stLogMessage(revision, data)

    println(s"Updating working copy: [${yellow(revision)}] $msg1st")      
    runCmd(Seq("svn", "update", s"--revision=$revision"))
  }
    

  //  We try to log the revision for the current working copy directory.
  //  If the revision does not exist in the repo we will get an exception
  //  If the revision does exist in the repo but is not on the trunk/branch for
  //  the working copy then we simply get an empty list.
  //  For HEAD, BASE, COMMITTED, PREV we have to specify a range and limit
  //  in order for subversion to return the log entry.
  private def resolveWorkingCopyRevision(rev: String): Option[Int] = {
    if (rev.isInteger) {
      Try(getSvnInfo(".", Some(rev)).commitRev) match {
        case Success(rev) => Some(rev.toInt)
        case Failure(_)   => None
      }
    }
    else {
      val cmdLine = Seq("svn", "log", "--xml", "--quiet", s"--revision=$rev:0", "--limit=1")
      Try(XML.loadString(runCmd(cmdLine).mkString("\n")) \ "logentry") match {
        case Success(nodes) if nodes.size == 0 => None
        case Success(nodes)                    => Some(parseLogEntry(nodes.head).revision.toInt)
        case Failure(_)                        => None
      }
    }
  }

  
  // Argument Types with associated argument parsers
  // ========================================================
  private case class RevisionArg(rev: String)     // Must be a valid revison in the working copy history
    
  private val revisionArgParser = (arg: String) => {
    val validRev = """^(?:\d+|HEAD|BASE|PREV|COMMITTED)$""".r
    if (!(validRev matches arg))
      throw new InvalidArgumentException(s" <revision> must be an integer or one of HEAD, BASE, PREV, COMMITTED")
    
    resolveWorkingCopyRevision(arg) match {
      case Some(rev) => RevisionArg(rev.toString)
      case None      => throw new InvalidArgumentException(s" this revision is not part of the working copy history")
    }
  }

  private case class RevisionRangeArg(low: Int, high: Int)
  
  private val revisionRangeArgParser = (arg: String) => {
    val revPart = """\d+|HEAD|BASE|PREV|COMMITTED"""
    val validRange = s"""^($revPart)(?::($revPart))?$$""".r
    
    arg match {
      case validRange(rev1, null) =>
        resolveWorkingCopyRevision(rev1) map (r => RevisionRangeArg(r, r)) getOrElse {
          throw new InvalidArgumentException(s" is not a valid <revision>")
        }

      case validRange(rev1, rev2) =>
        // Alwasy get range in order from hightest revision to lowest revision.
        (resolveWorkingCopyRevision(rev1), resolveWorkingCopyRevision(rev2)) match {
          case (Some(r1), Some(r2)) if r1 <= r2 => RevisionRangeArg(r1, r2)
          case (Some(r1), Some(r2)) => RevisionRangeArg(r2, r1)  //  Revs reversed (low to high)
            
          case _ =>
            throw new InvalidArgumentException(s" is not a subset of the working copy history")
        }
        
      case _  =>
        throw new InvalidArgumentException(s" is not a valid <revision> or <revision>:<revision>")
    }
  }

  private case class TermName(name: String)
  
  private val termArgParser = (arg: String) => {
    val validTerm = """^[A-Za-z][-_A-Za-z]*$""".r
    if (validTerm matches arg) {
      // The term must not match one of the standard bisect commmand names
      if (bisectCommands exists (_.cmdName == arg))
        throw new InvalidArgumentException(s" <term> cannot mask a built in bisect command name")
      else
      TermName(arg)
    }
    else
      throw new InvalidArgumentException(s" <term> must start with a letter and contain only letters, '-', or '_'")
  }
  
  private sealed trait BisectCommand {
    val cmdName: String
    def run(args: Seq[String]): Unit
  }
  
  // == Start Command ====================================================
  private case object Start extends BisectCommand {
    override val cmdName = "start"
      
    private case class Options(
      bad:      Option[String] = None,
      good:     Option[String] = None,
      termBad:  Option[String] = None,
      termGood: Option[String] = None)
        
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        val scriptPrefix = s"$scriptName $name"
        val cmdPrefix = s"$scriptPrefix $cmdName"
        
        addArgumentParser(revisionArgParser)
        addArgumentParser(termArgParser)
        
        banner = s"usage: $cmdPrefix [<options]"

        reqd[RevisionArg]("", "--bad=<revision>",    "The earliest revision that contains the bug")
          { (revision, options) => options.copy(bad = Some(revision.rev)) }
        reqd[RevisionArg]("", "--good=<revision>",   "The latest revision that does not contain the bug")
        { (revision, options) => options.copy(good = Some(revision.rev)) }
        reqd[TermName]("", "--term-bad=<term>",   "An alternate name for the 'bad' subcommand")
        { (term, options) => options.copy(termBad = Some(term.name)) }
        reqd[TermName]("", "--term-good=<term>",  "An alternate name for the 'good' subcommand")
        { (term, options) => options.copy(termGood = Some(term.name)) }
        
        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        separator("")
        separator(s"If you omit a bad revision, you must do so later with '$scriptPrefix bad <rev>' ")
        separator(s"If you omit a good revision, you must do so later with '$scriptPrefix good <rev>' ")
      }

      parser.parse(args, Options())
    }

    override def run(args: Seq[String]): Unit = {
      val options = processCommandLine(args)
      val cwd     = Paths.get("").toAbsolutePath
      val svnDir  = findSvnDir()
      val tmpDir  = new File(svnDir, "tmp")
      
      if (!tmpDir.isDirectory && !tmpDir.mkdir())
        generalError(s"Cannot acesss the temporary directory: $tmpDir")
      
      loadBisectData() match {
        case Some(data) =>
          System.err.println(s"$name already in progress")
          getWaitingStatus(data) foreach (s => System.err.println(s))
          System.err.println(s"\nType '$scriptName $name ${Reset.cmdName}' to reset your working copy")
          generalError(s"Type '$scriptName $name ${Reset.cmdName} --help' for more information")
          
        case None =>
          (options.bad map (_.toInt), options.good map (_.toInt)) match {
            case (Some(bad), Some(good)) if bad == good =>
              generalError("The 'bad' and 'good' revisions cannot be the same")
              
            case (Some(bad), Some(good)) if bad < good =>
              generalError("The 'good' revision must be an ancestor of the 'bad' revision")
              
            case _ =>
          }

          // If we have both bad and good then get the revisions between the two
          val badAndGood = (options.bad zip options.good).headOption
          val (maxRev, minRev, extantRevs) = badAndGood map { case (bad, good) =>
            val (maxR, minR, extants) = getExtantRevisions(bad, good)
            (Some(maxR), Some(minR), extants)
          } getOrElse {
            (options.bad, options.good, Seq.empty) 
          }
          
          // save the bisect data in order to start a new session.
          val data = BisectData(
            localPath   = cwd.toString,
            originalRev = getWorkingCopyInfo().commitRev,
            maxRev      = maxRev,
            minRev      = minRev,
            startMaxRev = maxRev,
            startMinRev = minRev,
            extantRevs  = extantRevs,
            termBad     = options.termBad,
            termGood    = options.termGood)
            
          saveBisectData(data)
          bisectLogFile.delete()  // Remove any previous log file.

          appendToBisectLog("#!/usr/bin/env sh")
          appendToBisectLog(s"# $scriptName $name log file  ${displayDateTime(LocalDateTime.now)}")
          appendToBisectLog(s"# Initiated from: $cwd")
          appendToBisectLog(s"# ----------------------------")
          data.maxRev foreach (r => logBisectRevision(r, data.termBadName, get1stLogMessage(r, Some(data))))
          data.minRev foreach (r => logBisectRevision(r, data.termGoodName, get1stLogMessage(r, Some(data))))
          getWaitingStatus(data) foreach { status =>
            appendToBisectLog(s"# $status")
            println(status)
          }          

          if (data.isReady)
            performBisect(data)
          
          logBisectCommand(cmdName +: args)
      }
    }
  }
  
  // == Bad Command ==++==================================================
  private case object Bad extends BisectCommand {
    override val cmdName = "bad"
    
    private case class Options(revision: Option[String] = None)
    
    private def processCommandLine(args: Seq[String], cmdTerm: String): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name $cmdTerm"
        
        addArgumentParser(revisionArgParser)
        
        banner = s"usage: $cmdPrefix [<revision>]"

        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        arg[RevisionArg] { (revision, options) => options.copy(revision = Some(revision.rev)) }  
            
        separator("")
        separator(s"Specify the earliest $cmdTerm revision")
        separator(s"The current working copy revision is used by default")
      }

      parser.parse(args, Options())
    }
    
    override def run(args: Seq[String]): Unit = {
      val data        = getBisectData()
      val options     = processCommandLine(args, data.termBadName)
      val revision    = options.revision getOrElse getWorkingCopyInfo().commitRev
      val minRev      = data.minRev map (_.toInt) getOrElse -1
      val startMaxRev = data.startMaxRev map (_.toInt) getOrElse Int.MaxValue
      
      // The new bad revision can come after the existing maxRev
      // This allows the user to recheck a range of commits.
      // The new bad revision cannot be less than or equal to the minRev
      if (revision.toInt <= minRev)
        println(s"'${data.termBadName}' revision must be more recent than the '${data.termGoodName}' revision")
      else if (revision.toInt > startMaxRev)
        println(s"'${data.termBadName}' revision is out of range, cannot come after starting '${data.termBadName}' revision ($startMaxRev)")
      else {
        markBadRevision(revision)
        logBisectCommand(data.termBadName +: args)
      }
      
      getWaitingStatus(getBisectData()) foreach { status =>
        appendToBisectLog(s"# $status")
        println(status)
      }          
    }
    
    //  Returns true if the performBisect() reports that the session is complete
    //  If this revision was previously skipped, it is no longer skipped
    def markBadRevision(revision: String): Boolean = {
      val data    = getBisectData()
      val newData = if (data.isReady) {
        data.copy(maxRev = Some(revision), skipped = data.skipped - revision)
      }
      else if (data.startMinRev.isDefined) {
        //  We are becoming ready
        val (maxRev, minRev, extantRevs) = getExtantRevisions(revision, data.startMinRev.get)
        data.copy(
          maxRev      = Some(maxRev),
          startMaxRev = Some(maxRev),
          minRev      = Some(minRev),
          startMinRev = Some(minRev),
          skipped     = data.skipped - revision,
          extantRevs  = extantRevs)
      }
      else {
        data.copy(
          maxRev      = Some(revision),
          startMaxRev = Some(revision),
          skipped     = data.skipped - revision)
      }
      
      saveBisectData(newData)
      logBisectRevision(revision, newData.termBadName, get1stLogMessage(revision, Some(newData)))
      if (newData.isReady)
        performBisect(newData)
      else
        false
    }
  }
  
  // == Good Command ==+==================================================
  private case object Good extends BisectCommand {
    override val cmdName = "good" 
    
    private case class Options(revision: Option[String] = None)
    
    private def processCommandLine(args: Seq[String], cmdTerm: String): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name $cmdTerm"
        
        addArgumentParser(revisionArgParser)
        
        banner = s"usage: $cmdPrefix [<revision>]"

        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        arg[RevisionArg] { (revision, options) => options.copy(revision = Some(revision.rev)) }  
            
        separator("")
        separator(s"Specify the earliest $cmdTerm revision")
        separator(s"The current working copy revision is used by default")
      }

      parser.parse(args, Options())
    }
    
    override def run(args: Seq[String]): Unit = {
      val data        = getBisectData()
      val options     = processCommandLine(args, data.termGoodName)
      val revision    = options.revision getOrElse getWorkingCopyInfo().commitRev
      val maxRev      = data.maxRev map (_.toInt) getOrElse Int.MaxValue
      val startMinRev = data.startMinRev map (_.toInt) getOrElse -1

      // The new good revision can come before the exisiing minRev
      // This allow the user to recheck a range of commits.
      // The new good revision cannot be greater than or equal to the maxRev
      if (revision.toInt >= maxRev)
        println(s"'${data.termGoodName}' revision must be older than the '${data.termBadName}' revision")
      else if (revision.toInt < startMinRev)
        println(s"'${data.termGoodName}' revision is out of range, cannot come before starting '${data.termGoodName}' revision ($startMinRev)")
      else {
        markGoodRevision(revision)
        logBisectCommand(data.termGoodName +: args)
      }
      
      getWaitingStatus(getBisectData()) foreach { status =>
        appendToBisectLog(s"# $status")
        println(status)
      }          
    }
    
    //  Returns true if the performBisect() reports that the session is complete
    //  If this revision was previously skipped, it is no longer skipped
    def markGoodRevision(revision: String): Boolean = {
      val data    = getBisectData()
      val newData = if (data.isReady) {
        data.copy(minRev = Some(revision), skipped = data.skipped - revision)
      }
      else if (data.startMaxRev.isDefined) {
        //  We are becoming ready
        val (maxRev, minRev, extantRevs) = getExtantRevisions(revision, data.startMinRev.get)
        data.copy(
          maxRev      = Some(maxRev),
          startMaxRev = Some(maxRev),
          minRev      = Some(minRev),
          startMinRev = Some(minRev),
          skipped     = data.skipped - revision,
          extantRevs  = extantRevs)
      }
      else {
        data.copy(
          minRev      = Some(revision),
          startMinRev = Some(revision),
          skipped     = data.skipped - revision)
      }

      saveBisectData(newData)
      logBisectRevision(revision, newData.termGoodName, get1stLogMessage(revision, Some(newData)))
      if (newData.isReady)
        performBisect(newData)
      else
        false
    }
  }
  
  // == Terms Command ====================================================
  private case object Terms extends BisectCommand {
    override val cmdName = "terms"
    
    private case class Options(termGood: Boolean = false, termBad: Boolean = false)
    
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name $cmdName"
        
        banner = s"usage: $cmdPrefix [--term-good|--term-bad]"

        flag("", "--term-good", "Display only the term for 'good'")
          { options =>
            if (options.termBad)
              throw new InvalidArgumentException(" - this command does not accept multiple options")
            options.copy(termGood = true)
          }
          
        flag("", "--term-bad", "Display only the term for 'bad'")
        { options =>
          if (options.termGood)
            throw new InvalidArgumentException(" - this command does not accept multiple options")
          options.copy(termBad = true)
        }
          
        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        separator("")
        separator(s"If no options are given then both terms are displayed")
        separator(s"The current working copy revision is used by default")
      }

      parser.parse(args, Options())
    }
    
    override def run(args: Seq[String]): Unit = {
      val data    = getBisectData()
      val options = processCommandLine(args)
      
      if (options.termGood)
        println(data.termGoodName)
      else if (options.termBad)
        println(data.termBadName)
      else {
        println(s"The term for the good state is ${blue(data.termGoodName)}")
        println(s"The term for the bad  state is ${blue(data.termBadName)}")
        getWaitingStatus(data) foreach println
      }
    }
  }
  
  private case class RevRange(low: Int, high: Int)
  
  private def rangesToSet(ranges: Seq[RevRange]): Set[String] = {
    ranges.foldLeft(Set.empty[String]) {
      case (combined, RevRange(low, high)) =>
      // Add 1 because the Set.range() function is exclusive on the high end
      combined ++ (Set.range(low, high+1) map (_.toString))
    }
  }
  
  // == Skip Command ====================================================+
  private case object Skip extends BisectCommand {
    override val cmdName = "skip"
    private case class Options(ranges: Seq[RevRange] = Seq.empty)
    
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name $cmdName"
        
        addArgumentParser(revisionRangeArgParser)
        
        banner = s"usage: $cmdPrefix [<revision>|<revision>:<revision>]..."

        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        arg[RevisionRangeArg] { 
          case (RevisionRangeArg(low, high), options) =>
            options.copy(ranges = options.ranges :+ RevRange(low, high))
        }
      }

      parser.parse(args, Options())
    }

    
    override def run(args: Seq[String]): Unit = {
      val options   = processCommandLine(args)
      val data      = getBisectData()
      val revisions = if (options.ranges.nonEmpty) rangesToSet(options.ranges) else Set(getWorkingCopyInfo().commitRev)
        
      markSkippedRevisions(revisions)
      logBisectCommand(cmdName +: args)
      
      getWaitingStatus(getBisectData()) foreach { status =>
        appendToBisectLog(s"# $status")
        println(status)
      }          
    }
    

    //  Returns true if the performBisect() reports that the session is complete
    def markSkippedRevisions(skipRevisions: Set[String]): Boolean = {
      val data       = getBisectData()
      val incoming   = candidatesOnly(data, skipRevisions)
      val newSkipped = (incoming -- data.skipped).toSeq sorted RevisionOrdering
      
      if (newSkipped.nonEmpty) {
        val newData = data.copy(skipped = data.skipped ++ incoming)
        saveBisectData(newData)
        newSkipped foreach (r => logBisectRevision(r, cmdName, get1stLogMessage(r, Some(newData))))
        if (newData.isReady)
          performBisect(newData)
        else
          false
      }
      else
        false
    }
  }
  
  // == Unskip Command ====================================================+
  private case object Unskip extends BisectCommand {
    override val cmdName = "unskip"
    
    private case class Options(ranges: Seq[RevRange] = Seq.empty)
    
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name $cmdName"
        
        addArgumentParser(revisionRangeArgParser)
        
        banner = s"usage: $cmdPrefix [<revision>|<revision>:<revision>]..."

        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        arg[RevisionRangeArg] {
          case (RevisionRangeArg(low, high), options) =>
            options.copy(ranges = options.ranges :+ RevRange(low, high))
        }
      }

      parser.parse(args, Options())
    }
    
    override def run(args: Seq[String]): Unit = {
      val options      = processCommandLine(args)
      val data         = getBisectData()
      val revisions    = if (options.ranges.nonEmpty) rangesToSet(options.ranges) else Set(getWorkingCopyInfo().commitRev)
        
      markUnskippedRevision(revisions)
      logBisectCommand(cmdName +: args)
            
      getWaitingStatus(getBisectData()) foreach { status =>
        appendToBisectLog(s"# $status")
        println(status)
      }          
    }
    
    //  Returns true if the performBisect() reports that the session is complete
    def markUnskippedRevision(unskipRevisions: Set[String]): Boolean = {
      val data         = getBisectData()
      val incoming     = candidatesOnly(data, unskipRevisions)
      val newUnskipped = (incoming intersect data.skipped).toSeq sorted RevisionOrdering
      
      if (newUnskipped.nonEmpty) {
        val newData = data.copy(skipped = data.skipped -- incoming)
        saveBisectData(newData)
        newUnskipped foreach (r => logBisectRevision(r, cmdName, get1stLogMessage(r, Some(newData))))
        
        if (newData.isReady)
          performBisect(newData)
        else
          false
      }
      else
        false
    }
  }
  
  // == Run Command ======================================================
  private case object Run extends BisectCommand {
    override val cmdName = "run"
    val cmdPrefix = s"$scriptName $name $cmdName"

    case class Options(cmdArgs: Seq[String] = Seq.empty)
    
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        
        banner = s"usage: $cmdPrefix <cmd> [<arg>...]"

        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        arg[String] { (cmdArg, options) => 
          options.copy(cmdArgs = options.cmdArgs :+ cmdArg)
        }
      }

      parser.parse(args, Options())
    }

    //  Run the supplied command continuously until we reach
    //  the end of the bisect session and find the target revision
    //  
    //  If the command returns:
    //  ------------------------------------------------------------
    //  0                      The current commit is good
    //  125                    The curent commit should be skipped
    //  1 - 127 (except 125)   The current commit is bad
    //  128 - 255              The bisect session should be aborted
     
    override def run(args: Seq[String]): Unit = {
      val options = processCommandLine(if (args.isEmpty) Seq("--help") else args)
      val initialData = getBisectData()

      if (options.cmdArgs.isEmpty)
        generalError("You must specify a command to run")
      
      getWaitingStatus(initialData) foreach println
      
      if (!initialData.isReady)
        generalError(s"$cmdPrefix cannot be used until a '${initialData.termGoodName}' revision and '${initialData.termBadName}' revision have been supplied")

      def runCommand(): Unit = {
        val data = getBisectData()  // Get fresh data

        println(options.cmdArgs mkString " ")
        val finished = exec(options.cmdArgs, None, new ConsoleExecLogger) match {
          case 0 =>
            displayBisectCommand(Seq(data.termGoodName))
            val complete = Good.markGoodRevision(getWorkingCopyInfo().commitRev)
            logBisectCommand(Seq(data.termGoodName))
            complete
            
          case 125 =>
            displayBisectCommand(Seq(Skip.cmdName))
            val complete = Skip.markSkippedRevisions(Set(getWorkingCopyInfo().commitRev))
            logBisectCommand(Seq(Skip.cmdName))
            complete
          
          case r if r < 128 =>
            displayBisectCommand(Seq(data.termBadName))
            val complete = Bad.markBadRevision(getWorkingCopyInfo().commitRev)
            logBisectCommand(Seq(data.termBadName))
            complete
            
          case r => generalError(s"$cmdPrefix: failed.  Command '${options.cmdArgs.head}' returned unrecoverable error code ($r)")
        }
        
        if (!finished)
          runCommand()
      }

      // Start it off
      runCommand()
    }
  }
    
  // == Reset Command ====================================================
  private case object Reset extends BisectCommand {
    override val cmdName = "reset"

    private case class Options(update: Boolean = true, revision: Option[String] = None)
    
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name $cmdName"
        
        addArgumentParser(revisionArgParser)
        
        banner = s"usage: $cmdPrefix [<options>] [<revision>]"

        flag("", "--no-update",    "Do not update working copy")
          { _.copy(update = false) }
          
        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        arg[RevisionArg] { (revision, options) => options.copy(revision = Some(revision.rev)) }  
            
        separator("")
        separator(s"The default is to update your working copy to its original revision before the bisect")
        separator(s"If a <revision> is specified, then the working copy will be updated to it insstead")
        separator(s"You can also elect to keep your working copy as it is with --no-update")
      }

      parser.parse(args, Options())
    }

    override def run(args: Seq[String]): Unit = {
      val options = processCommandLine(args)
      val data    = getBisectData()
      
      if (options.update) {
        val updateRev = options.revision getOrElse data.originalRev
        updateWorkingCopy(updateRev, Some(data))
      }
      else {
        val currentRev = getWorkingCopyInfo().commitRev
        val msg1st     = get1stLogMessage(currentRev, Some(data))
        
        println(s"Working copy: [${yellow(currentRev)}] $msg1st")      
      }
        
      //  Remove the data file, this will clear the bisect session
      bisectDataFile.delete()
      bisectLogFile.delete()
    }
  }
  
  // == Log Command ======================================================
  private case object Log extends BisectCommand {
    override val cmdName = "log"

    private def processCommandLine(args: Seq[String]): Unit = {

      val parser = new OptionParser[Unit] {
        val cmdPrefix = s"$scriptName $name $cmdName"
        
        banner = s"usage: $cmdPrefix [<options>]"

        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
      }

      parser.parse(args, ())
    }

    override def run(args: Seq[String]): Unit = {
      processCommandLine(args)
      getBisectData() // Make sure a bisect session has been started
      displayBisectLog()
    }
  }
    
  // == Replay Command ===================================================
  private case object Replay extends BisectCommand {
    override val cmdName = "replay"
    
    private case class Options(logFile: Option[File] = None)
    
    private def processCommandLine(args: Seq[String]): Options = {

      val parser = new OptionParser[Options] {
        val cmdPrefix = s"$scriptName $name $cmdName"
        
        addArgumentParser(revisionArgParser)
        
        banner = s"usage: $cmdPrefix [<options>] <log file>"

        flag("-h", "--help", "Show this message")
            { _ => println(help); throw HelpException() }
            
        arg[File] { (value, options) => options.copy(logFile = Some(value)) }  
      }

      parser.parse(args, Options())
    }

    override def run(args: Seq[String]): Unit = {
      val options = processCommandLine(args)
      
      options.logFile match {
        case None       =>
          generalError("You must specify a log file to replay")
          
        case Some(file) if !file.exists =>
          generalError(s"File '$file' does not exist")
          
        case Some(file) =>
          val cmdLine = Seq("/bin/sh", "-c", file.toString)
          exec(cmdLine, None, new ConsoleExecLogger)
      }
    }
  }
    
    
  private val bisectCommands = Start::Bad::Good::Terms::Skip::Unskip::Run::Log::Replay::Reset::Nil
  
  private def matchCommand(cmdName: String, cmdList: List[String]): List[String] = {
    if ("""^[a-zA-Z][-a-zA-Z0-9_]*""".r matches cmdName)
      cmdList filter (_ startsWith cmdName)
    else
      Nil
  }
  
  def showHelp(): Nothing = {
    val sv = scriptName
    val help = s"""|Available bisect commands:
                   |$sv $name start       Start a bisect session in the current subversion
                   |                      working copy directory
                   |$sv $name bad         Mark a revision as bad  (It contains the bug)
                   |$sv $name good        Mark a revision as good  (It does not contain the bug)
                   |$sv $name terms       Show the currently defined terms for good/bad
                   |$sv $name skip        Skip a revision.  It will no longer be considered
                   |$sv $name unskip      Reinstate a previously skipped revision
                   |$sv $name run         Automate the bisect session by running a script
                   |                      for each tested revision 
                   |$sv $name log         Show the bisect log
                   |$sv $name replay      Replay the bisect session from a log file
                   |$sv $name reset       Clean up after a bisect session returning the working
                   |                      copy to its original revision
                   |
                   |Type '$sv $name <command> --help' for details on a specific command""".stripMargin
      println(help)
      throw HelpException()
  }
  
  private def getBisectCommand(cmdName: String, termBad: Option[String], termGood: Option[String]): BisectCommand = {
    val cmdList = bisectCommands.map(_.cmdName) ::: termBad.toList ::: termGood.toList
    matchCommand(cmdName, cmdList) match {
      case Nil                                   => showHelp()
      case name :: Nil if Some(name) == termBad  => Bad
      case name :: Nil if Some(name) == termGood => Good
      case name :: Nil                           => bisectCommands.find(_.cmdName == name).get
      case names =>
        generalError(s"$scriptName $name command '$cmdName' is ambiguous.  (${names.mkString(", ")})")
    }      
  }

  // Main entry point to bisect commnad
  override def run(args: Seq[String]): Unit = {

    if (args.isEmpty || args.head == "help" || args.head == "--help")
      showHelp();
    else {
      val wcInfo  = getWorkingCopyInfo()
      val optData = loadBisectData()
      val command = getBisectCommand(args.head, optData flatMap (_.termBad), optData flatMap (_.termGood))
      command.run(args.tail)
    }
  } 
}
