
package svutil

import java.util.Locale
import java.time.format.DateTimeFormatter
import java.io.{ FileWriter, PrintWriter }
import scala.util.Properties.{ propOrNone, envOrNone, isWin }
import java.time._
import scala.xml._
import Utilities._
import Color._

//  Functions that execute svn and parse the results.
object svn {

  //  By default we use the `svn` command found on the user's PATH
  //  But this can be overridden by an environment variable or java property.
  lazy val svnCmd = envOrNone("SV_SVN") orElse propOrNone("sv.svn") getOrElse "svn"
  
  // ==========================================
  // svn info
  // ==========================================
  object model {
    
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
      workingCopyLastUpdate: Option[LocalDateTime]  // Only if the referred to a working copy (not URL)
    ) 
      
    // ==========================================
    // svn log entries
    // ==========================================
    case class FromPath(path: String, revision: String)
    case class LogPath(path: String, kind: String, action: String, textMods: Boolean, propMods: Boolean, fromPath: Option[FromPath]) {
      def formatted:String = {
        val color = action match {
          case "D" => red
          case "A" => green
          case "M" => purple
          case _   => white
        }
        val from = fromPath match {
          case Some(FromPath(path, revision)) => s"  (from ${path}:${revision})"
          case None                           => ""
        }
        s"  ${color(action)} ${color(path)}${purple(from)}"
      }
    }
    case class LogEntry(revision: String, author: String, date: LocalDateTime, msg: Seq[String], paths: Seq[LogPath])
      
  
    // ==========================================
    // svn list entries
    // ==========================================
    case class ListEntry(name: String, kind: String, size: Option[Long], commitRev: String, commitAuthor: String, commitDate: LocalDateTime)
    case class SvnList(path: String, entries: List[ListEntry])
  
      
    // ==========================================
    // svn status entries
    // ==========================================
    case class StatusEntry(path: String, itemStatus: String, propsStatus: String, revision: String)
    case class SvnStatus(path: String, entries: Seq[StatusEntry])
      
    case class SearchOption(glob: String, searchAnd: Boolean = false)
  }

  import model._
  
  //  Verifiy that the currrent working directory is an SVN working copy
  def workingCopyInfo: SvnInfo = {
    try svn.info(".")
    catch {
      case ExecError(_, _) =>
        generalError(s"This command must be run from within a subversion working copy directory.")
        
      case e: Throwable =>
        generalError("Error verifying working copy\n" + (Option(e.getMessage) getOrElse e.getClass.getName))
    }
  }

  //  Return true if the current working directory is within
  //  a subverion working copy directory tree
  def inWorkingCopy: Boolean = {
      try {
        workingCopyInfo
        true
      }
      catch {
        case GeneralError(_) => false
      }
  }

  //  Starting in the current working directory search for the top
  //  of the working copy.  The directory that contains the .svn directory.
  def workingCopyRoot: Option[os.Path] = {
      def findIt(path: os.Path): Option[os.Path] =
        (os.isDir(path / ".svn"), path.segmentCount) match {
          case (false, 0) => None
          case (false, _) => findIt(path / os.up)
          case (true,  _) => Some(path)
        }
      
      // Start with the current working directory path
      findIt(os.pwd)
  }
  
  
  private [svn] def parseSvnInfo(entry: Node): SvnInfo = {
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
      commitAuthor          = (commit \ "author").headOption map (_.text) getOrElse "n/a",
      commitDate            = (commit \ "date").headOption map (d => parseISODate(d.text)) getOrElse NULL_DATETIME,
      workingCopyPath       = (entry \ "wc-info" \ "wcroot-abspath").headOption map (_.text),
      workingCopyLastUpdate = (entry \ "wc-info" \ "text-updated").headOption map (d => parseISODate(d.text)))
  }
  
  //  Get info for a single path
  def info(path: String, revision: Option[String] = None): SvnInfo = {
    val revArg  = revision map (r => Seq("--revision", r)) getOrElse Seq.empty
    val cmdLine = Seq(svnCmd, "info", "--xml") ++ revArg :+ path
    val out     = runCmd(cmdLine)
    val entry   = (XML.loadString(out.mkString("\n")) \ "entry").head
    parseSvnInfo(entry)
  }
  
  // Get info for multiple paths
  def infoList(paths: Seq[String], revision: Option[String] = None): Seq[SvnInfo] = {
    val revArg  = revision map (r => Seq("--revision", r)) getOrElse Seq.empty
    val cmdLine = Seq(svnCmd, "info", "--xml") ++ revArg ++ paths
    val out     = runCmd(cmdLine)
    val entries = (XML.loadString(out.mkString("\n")) \ "entry")
    
    entries.toSeq map parseSvnInfo
  }


  // Using svn info for the path, return the branch name and the current commit revision
  // Assumes the repo has the standard ^{trunk,branches,tags} structure
  def currentBranch(path: String): (String, String) = {

    val pathInfo = info(path)
    val TRUNK    = """\^.*/trunk.*""".r
    val BRANCH   = """\^.*/branches/([^/]+).*""".r
    val TAG      = """\^.*/tags/([^/]+).*""".r
    // Parse the XML log entries
    val branch = pathInfo.relativeUrl match {
      case TRUNK()      => "trunk"
      case BRANCH(name) => s"$name"
      case TAG(name)    => s"$name"
      case _            => "cannot be determined"
    }
    (branch, pathInfo.commitRev)
  }

  
    
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
      author   = (entry \ "author").headOption map (_.text) getOrElse "n/a",
      date     = (entry \ "date").headOption map (d => parseISODate(d.text)) getOrElse NULL_DATETIME,
      msg      = (entry \ "msg").headOption map { _.text.split("\n").toSeq } getOrElse Seq.empty ,
      paths    = pathEntries)
  }
  
  def pathList(paths: String*): List[SvnList] = {
    if (paths.isEmpty)
      List.empty
    else {
      val cmdLine   = Seq(svnCmd, "list", "--xml") ++ paths
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
            commitAuthor = (commit \ "author").headOption map (_.text) getOrElse "n/a",
            commitDate   = (commit \ "date").headOption map (d => parseISODate(d.text)) getOrElse NULL_DATETIME
          )
        }
        
        SvnList(path, entries)
      }
    }
  }
  
    // cwd specifies the directory where the command will run.
  // This affcts the path values that are returned by the svn status command
  // If the path argument is an absolute path, then the paths returned will also be absolute
  // If the path argument is a relative path, then the path of each entry will be relateive
  // to the givne path.
  def status(path: String, cwd: Option[os.Path]): SvnStatus = {
    val cmdLine    = Seq(svnCmd, "status", "--xml", path)
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
  
  //  Returns the diff for the changes introduced by the given revision
  def changeDiff(pathOrUrl: String, commitRev: String): Seq[String] = {
    runCmd(Seq(svnCmd, "diff", "--change", commitRev, pathOrUrl))
  }

  def log(
    paths: Seq[String] = Seq.empty,
    revisions: Seq[String] = Seq.empty,
    includeMessage: Boolean = true,
    limit: Option[Int] = None,
    stopOnCopy: Boolean = false,
    includePaths: Boolean = false,
    searchOptions: Seq[SearchOption] = Seq.empty
  ): Seq[LogEntry] = {
    var cmdLine = Vector[String](svnCmd, "log", "--xml")

    if (!includeMessage) cmdLine :+= "--quiet"
    if (stopOnCopy)     cmdLine :+= "--stop-on-copy"
    if (includePaths)    cmdLine :+= "--verbose"
    cmdLine :++= limit.toSeq.map(l => s"--limit=$l")
    cmdLine :++= revisions.map(r => s"--revision=$r")
    cmdLine :++= searchOptions.map {
      case SearchOption(glob, false) => s"--search=$glob"
      case SearchOption(glob, true)  => s"--search-and=$glob"
    }
    cmdLine :++= paths
    val xml = XML.loadString(runCmd(cmdLine).mkString("\n"))
    (xml \ "logentry") map parseLogEntry
  }
  
  def update(revision: String, depth: String = "infinity", cwd: Option[os.Path] = None): Seq[String] = {
    runCmd(Seq(svnCmd, "update", s"--depth=$depth", s"--revision=$revision"), cwd)
  }

  def revert(paths: Seq[String], depth: String = "infinity", removeAdded: Boolean = false, cwd: Option[os.Path] = None): Seq[String] = {
    val rmArg = if (removeAdded) Seq("--remove-added") else Seq.empty
    val cmdLine = Seq(svnCmd, "revert", s"--depth=$depth") ++ rmArg ++ paths
    runCmd(cmdLine, cwd)
  }
  
  def createPatch(patchFile: os.Path, workingDirPath: String = ".", depth: String = "infinity", cwd: Option[os.Path] = None): Unit = {
    val cmdLine = Seq(svnCmd, "diff", s"--depth=$depth", "--ignore-properties", workingDirPath)
    val diffOut = runCmd(cmdLine, cwd)
    val file    = patchFile.toIO
    
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

  def applyPatch(patchFile: os.Path, dryRun: Boolean = false, cwd: Option[os.Path] = None): Seq[String] = {
    val cmdLine   = if (dryRun) 
      Seq(svnCmd, "patch", "--dry-run", patchFile.toString)
    else
      Seq(svnCmd, "patch", patchFile.toString)
    
    runCmd(cmdLine, cwd)
  }
  
  def add(paths: Seq[String], depth: String = "infinity", noAutoProps: Boolean = false, cwd: Option[os.Path] = None): Seq[String] = {
    val propsArg = if (noAutoProps) Seq("--no-auto-props") else Seq.empty
    val cmdLine = Seq(svnCmd, "add", s"--depth=$depth") ++ propsArg ++ paths
    runCmd(cmdLine, cwd)        
  }
    
}


