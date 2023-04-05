
package svutil

import java.util.Locale
import java.time.format.DateTimeFormatter
import java.nio.file.{ Files, Paths, Path }
import scala.util.Properties.propOrElse
import java.time._
import java.io.File
import com.typesafe.config.Config
import scala.xml._
import Exec._
import Color._

object Utilities {
  
  lazy val scriptPath = propOrElse("sv.scriptname", "sv")
  lazy val scriptName = new File(scriptPath).getName
  
  case class GeneralError(message: String = "") extends Exception(message)
  case class HelpException() extends Exception("Help Exception")
  case class SuccessExit(message: String = "") extends Exception(message)
  
  def generalError(msg: String): Nothing = throw GeneralError(msg)
  
  def successExit(msg: String): Nothing = throw SuccessExit(msg)
  
  implicit class StringWrapper(str: String) {
    def chomp(suffix: String = "\n"): String = suffix match {
      case null | "" => str
      case "\n" if str.endsWith("\r\n") => str.reverse.drop(2).reverse.toString
      case "\n" if str.endsWith("\r") || str.endsWith("\n") => str.reverse.drop(1).reverse.toString
      case suf   if str.endsWith(suf) => str.reverse.drop(suf.length).reverse.toString
      case _ => str
    }
    def isInteger = str forall (_.isDigit)
  }


  implicit class ConfigWrapper(cfg: Config) {
    def optString(path: String): Option[String] = if (cfg.getIsNull(path)) None else Some(cfg.getString(path))
  }
    
  
  def joinPaths(base: String, others: String*): String = {
    val result = new StringBuilder(base.chomp("/"))
    for (segment <- others)
      result.append("/").append(segment.chomp("/"))
    
    result.result()
  }
  
  val ISODateFormat     = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.nX")
  val DisplayDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  val DisplayTimeFormat = DateTimeFormatter.ofPattern("HH:mm:ss")
  
  //  The svn XML output returns dates in ISO format in the UTC time zone
  //  We parse them and convert them to local date and time
  def parseISODate(isoDate: String): LocalDateTime = {
    val utc = ZonedDateTime.parse(isoDate, ISODateFormat)
    utc.withZoneSameInstant(ZoneId.systemDefault).toLocalDateTime
  }
  
  //  Convert the local date to a utc and format it as a string
  def toISODateString(date: LocalDateTime): String = {
    val zonedDate = date.atZone(ZoneId.systemDefault)
    val utcDate   = zonedDate.withZoneSameInstant(ZoneId.ofOffset("", ZoneOffset.UTC))    
    utcDate.format(ISODateFormat)
  }
  
  def displayDate(date: LocalDateTime): String = date.format(DisplayDateFormat)
  def displayTime(date: LocalDateTime): String = date.format(DisplayTimeFormat)
  def displayDateTime(date: LocalDateTime): String = s"${displayDate(date)} ${displayTime(date)}"


  //  Verifiy that the currrent working directory is an SVN working copy
  def getWorkingCopyInfo(): SvnInfo = {
    try getSvnInfo(".")
    catch {
      case ExecError(_, _) =>
        generalError(s"This command must be run from within a subversion working copy directory.")
        
      case e: Throwable =>
        generalError("Error verifying working copy\n" + (Option(e.getMessage) getOrElse e.getClass.getName))
    }
  }


  //  Starting in the current working directory search for the top
  //  of the working copy.  The directory that contains the .svn directory.
  def getWorkingCopyRoot(): Option[Path] = {

      def findIt(path: Path): Option[Path] =
        if (path == null)
          None
        else 
          path.resolve(".svn").toFile match {
            case svn if svn.isDirectory => Some(path)
            case _                      => findIt(path.getParent)
          }
      
      // Start with the current working directory path
      findIt(Paths.get("").toAbsolutePath)
  }
  
  
  //  We create a .sv directory in the top directory of the working copy
  //  This gives sv commands a place to store data
  //  This will throw an error of the directory cannot be resloved.
  def getDataDirectory(): Path = {
    val wcRoot  = getWorkingCopyRoot() getOrElse {
      generalError(s"You must run this command from within a subversion working copy directory")
    }
    
    val dotSVDir = wcRoot.resolve( ".sv").toFile
    if (dotSVDir.isDirectory || dotSVDir.mkdir())
      dotSVDir.toPath
    else
      generalError(s"Unable to create the .sv directory in your working copy")
  }
  


  // ==========================================
  // SVN Info
  // ==========================================
  case class SvnInfo(
    path: String,  // Not too useful
    repoRev: String,
    kind: String,
    size: Option[Long],
    url: String,
    relativeUrl: String,
    rootUrl: String,  // URL to repo.  Up to but not including /trunk..., /branches..., /tags...
    repoUUID: String,
    commitRev: String,
    commitAuthor: String,
    commitDate: LocalDateTime,
    workingCopyPath: Option[String],              // Only if the referred to a working copy (not URL)
    workingCopyLastUpdate: Option[LocalDateTime]) // Only if the referred to a working copy (not URL)
      
  def inspect[T](s: String, x: T): T = {
    println(s"INSPECT $s=$x")
    x
  }
  
  def parseSvnInfo(entry: Node): SvnInfo = {
    val commit  = (entry \ "commit").head

    SvnInfo(
      path                  = entry.attributes("path").head.text,
      repoRev               = entry.attributes("revision").head.text,
      kind                  = entry.attributes("kind").head.text,
      size                  = Option(entry.attributes("size")) map (_.text.toLong),
      url                   = (entry \ "url").head.text,
      relativeUrl           = (entry \ "relative-url").head.text,
      rootUrl               = (entry \ "repository" \ "root").head.text,
      repoUUID              = (entry \ "repository" \ "uuid").head.text,
      commitRev             = commit.attributes("revision").head.text,
      commitAuthor          = (commit \ "author").head.text,
      commitDate            = parseISODate((commit \ "date").head.text),
      workingCopyPath       = (entry \ "wc-info" \ "wcroot-abspath").headOption map (_.text),
      workingCopyLastUpdate = (entry \ "wc-info" \ "text-updated").headOption map (x => parseISODate(x.text)))
  }
  
  def getSvnInfo(path: String, revision: Option[String] = None): SvnInfo = {
    val revArg  = revision map (r => Seq("--revision", r)) getOrElse Seq.empty
    val cmdLine = Seq("svn", "info", "--xml") ++ revArg :+ path
    val out     = runCmd(cmdLine)
    val entry   = (XML.loadString(out.mkString("\n")) \ "entry").head
    parseSvnInfo(entry)
  }
  
  def getSvnInfoList(paths: Seq[String], revision: Option[String] = None): Seq[SvnInfo] = {
    val revArg  = revision map (r => Seq("--revision", r)) getOrElse Seq.empty
    val cmdLine = Seq("svn", "info", "--xml") ++ revArg ++ paths
    val out     = runCmd(cmdLine)
    val entries = (XML.loadString(out.mkString("\n")) \ "entry")
    
    entries.toSeq map parseSvnInfo
  }


  //  Using svn info for the path, return the branch name and the current commit revision
  def getCurrentBranch(path: String): (String, String) = {

    val info = getSvnInfo(path)
    val TRUNK  = """\^.*/trunk.*""".r
    val BRANCH = """\^.*/branches/([^/]+).*""".r
    val TAG    = """\^.*/tags/([^/]+).*""".r
    // Parse the XML log entries
    val branch = info.relativeUrl match {
      case TRUNK()      => "trunk"
      case BRANCH(name) => s"$name"
      case TAG(name)    => s"$name"
      case _            => "cannot be determined"
    }
    (branch, info.commitRev)
  }

  
  // ==========================================
  // SVN Log Entry
  // ==========================================
  case class FromPath(path: String, revision: String)
  case class LogPath(path: String, kind: String, action: String, textMods: Boolean, propMods: Boolean, fromPath: Option[FromPath]) {
    def formatted:String = {
      val color = action match {
        case "D" => red _
        case "A" => green _
        case "M" => purple _
        case _   => white _
      }
      val from = fromPath match {
        case Some(FromPath(path, revision)) => s"  (from ${path}:${revision})"
        case None                           => ""
      }
      s"  ${color(action)} ${color(path)}${purple(from)}"
    }
  }
  case class LogEntry(revision: String, author: String, date: LocalDateTime, msg: Seq[String], paths: Seq[LogPath])
    
  def parseLogEntry(entry: Node): LogEntry = {
    val pathNodes = (entry \ "paths" \ "path").toSeq
    val pathEntries = pathNodes map { pathNode =>
      val fromPath = if (pathNode.attributes("copyfrom-path") != null) {
        Some(FromPath(pathNode.attributes("copyfrom-path").head.text, pathNode.attributes("copyfrom-rev").head.text))
      }
      else
        None
      LogPath(
        path     = pathNode.head.text,
        kind     = pathNode.attributes("action").head.text,
        action   = pathNode.attributes("action").head.text,
        textMods = pathNode.attributes("text-mods").head.text == "true",
        propMods = pathNode.attributes("prop-mods").head.text == "true",
        fromPath = fromPath)
    }
    
    LogEntry(
      revision = entry.attributes("revision").head.text,
      author   = (entry \ "author").head.text,
      date     = parseISODate((entry \ "date").head.text),
      msg      = (entry \ "msg").headOption map { _.text.split("\n").toSeq } getOrElse Seq.empty ,
      paths    = pathEntries)
  }
  
  // ==========================================
  // SVN List Entry
  // ==========================================
  case class ListEntry(name: String, kind: String, size: Option[Long], commitRev: String, commitAuthor: String, commitDate: LocalDateTime)
  case class SvnList(path: String, entries: List[ListEntry])
  
  def getSvnLists(paths: String*): List[SvnList] = {
    if (paths.isEmpty)
      List.empty
    else {
      val cmdLine   = Seq("svn", "list", "--xml") ++ paths
      val lsOut     = runCmd(cmdLine)
      val listNodes = (XML.loadString(lsOut.mkString("\n")) \ "list")
      
      for (listNode <- listNodes.toList) yield {
        val path = listNode.attributes("path").head.text
        
        val entries = for (entryNode <- (listNode \ "entry").toList) yield {
          val commit = (entryNode \ "commit").head
          ListEntry(
            name         = (entryNode \ "name").head.text,
            kind         = entryNode.attributes("kind").head.text,
            size         = (entryNode \ "size").headOption map (_.text.toLong),
            commitRev    = commit.attributes("revision").head.text,
            commitAuthor = (commit \ "author").headOption map (_.text) getOrElse "",
            commitDate   = parseISODate((commit \ "date").head.text))
        }
        
        SvnList(path, entries)
      }
    }
  }
  
  
  def showCommit(logEntry: LogEntry, showMsg: Boolean = true, showPaths: Boolean = true): Unit = {
    println("-------------------------------------------------------------------")
    println(s"Commit: ${yellow(logEntry.revision)}")
    println(s"Author: ${cyan(logEntry.author)}")
    println(s"Date  : ${purple(displayDateTime(logEntry.date))}")
    println("-------------------------------------------------------------------")

    if (showMsg)
      logEntry.msg foreach (m => println(s"  $m"))
  
    if (showPaths)
      logEntry.paths foreach (p => println(p.formatted))
  }
  
  def printDiffLine(line: String): Unit = {
    val color: (String) => String = {
      if      ((line startsWith "---") || (line startsWith "+++")) blue _
      else if (line startsWith "Index:")                           yellow _
      else if (line startsWith "==========")                       yellow _
      else if (line startsWith "Property changes on:")             purple _
      else if (line startsWith "+")                                green _
      else if (line startsWith "@@")                               gray _
      else if (line startsWith "-")                                red _
      else                                                         white _ 
    }

    println(color(line))
  }
  
  
  def showChangeDiff(url: String, commitRev: String): Unit = {
    val out = runCmd(Seq("svn", "diff", "--change", commitRev, url))

    println()
    out foreach printDiffLine
  }
  
  case class StatusEntry(path: String, itemStatus: String, propsStatus: String, revision: String)
  case class SvnStatus(path: String, entries: Seq[StatusEntry])

  // cwd specifies the directory where the command will run.
  // This affcts the path values that are returned by the svn status command
  // If the path argument is an absolute path, then the paths returned will also be absolute
  // If the path argument is a relative path, then the path of each entry will be relateive
  // to the givne path.
  def getSvnStatus(path: String, cwd: Option[File]): SvnStatus = {
    val cmdLine    = Seq("svn", "status", "--xml", path)
    val out        = runCmd(cmdLine, cwd)
    val targetNode = (XML.loadString(out.mkString("\n")) \ "target").head
    val entryNodes = (targetNode \ "entry")

    val entries = for (entry <- entryNodes.toSeq) yield {
      val wcNode = (entry \ "wc-status").head
      // unversioned entries will not have a revision
      val revision = if (wcNode.attributes("revision") == null) "-1" else wcNode.attributes("revision").head.text
      StatusEntry(
        path        = entry.attributes("path").head.text,
        itemStatus  = wcNode.attributes("item").head.text,
        propsStatus = wcNode.attributes("props").head.text,
        revision    = revision)
    }
    SvnStatus(targetNode.attributes("path").head.text, entries)
  }
}


