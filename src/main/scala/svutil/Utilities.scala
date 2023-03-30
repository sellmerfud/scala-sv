
package svutil

import java.util.Locale
import java.time.format.DateTimeFormatter
import scala.util.Properties.propOrElse
import java.time._
import java.io.File
import scala.xml._
import Exec._
import Color._
import exceptions._

object Utilities {
  
  lazy val scriptPath = propOrElse("sv.scriptname", "sv")
  lazy val scriptName = new File(scriptPath).getName
  
  
  def generalError(msg: String): Nothing = throw new GeneralError(msg)
  
  implicit class StringWrapper(str: String) {
    def chomp(suffix: String = "\n"): String = suffix match {
      case null | "" => str
      case "\n" if str.endsWith("\r\n") => str.reverse.drop(2).reverse.toString
      case "\n" if str.endsWith("\r") || str.endsWith("\n") => str.reverse.drop(1).reverse.toString
      case suf   if str.endsWith(suf) => str.reverse.drop(suf.length).reverse.toString
      case _ => str
    }
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
  
  def displayDate(date: LocalDateTime): String = date.format(DisplayDateFormat)
  def displayTime(date: LocalDateTime): String = date.format(DisplayTimeFormat)
  def displayDateTime(date: LocalDateTime): String = s"${displayDate(date)} ${displayTime(date)}"

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

  
  // ==========================================
  // SVN Log Entry
  // ==========================================
  case class FromPath(path: String, revision: String)
  case class LogPath(path: String, kind: String, action: String, textMods: Boolean, propMods: Boolean, fromPath: Option[FromPath]) {
    def formatted:String = {
      val coloredAction = action match {
        case "D" => red("D")
        case "A" => green("A")
        case _   => white(action)
      }
      val from = fromPath match {
        case Some(FromPath(path, revision)) => s"  (from ${path}:${revision})"
        case None                           => ""
      }
      s"  ${coloredAction} ${blue(path)}${red(from)}"
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
  // SVN ist Entry
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
            commitAuthor = (commit \ "author").head.text,
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
  
  def showChangeDiff(url: String, commitRev: String): Unit = {
    val out = runCmd(Seq("svn", "diff", "--change", commitRev, url))

    def lineColor(line: String): (String) => String = {
      if      ((line startsWith "---") || (line startsWith "+++")) blue _
      else if (line startsWith "Index:")                           yellow _
      else if (line startsWith "==========")                       yellow _
      else if (line startsWith "Property changes on:")             purple _
      else if (line startsWith "+")                                green _
      else if (line startsWith "@@")                               gray _
      else if (line startsWith "-")                                red _
      else                                                         white _ 
    }

    println()
    for (line <- out)
      println(lineColor(line)(line))
  }
  
}


