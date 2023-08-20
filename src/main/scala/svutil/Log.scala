

package svutil

import scala.util.matching.Regex
import java.time.LocalDateTime
import Color._
import Utilities._
import svn.model.{ LogEntry, SearchOption }

object Log extends Command {
  
  override val name = "log"
  override val description = "Display formatted log entries"
  case class Options(
    limit:     Option[Int]          = None,
    author:    Boolean              = false,
    date:      Boolean              = false,
    time:      Boolean              = false,
    full:      Boolean              = false,
    showPaths: Boolean              = false,
    noCopy:    Boolean              = false,
    incoming:  Boolean              = false,
    reverse:   Boolean              = false,
    revisions: Vector[String]       = Vector.empty,
    search:    Vector[SearchOption] = Vector.empty,
    regexes:   Vector[Regex]        = Vector.empty,
    paths:     Vector[String]       = Vector.empty)



  private def processCommandLine(args: Seq[String]): Options = {
    import org.sellmerfud.optparse._
    import java.util.regex.PatternSyntaxException
    
    val parser = new OptionParser[Options] {
      
      addArgumentParser[Regex] { arg =>
        try   { new Regex(arg) }
        catch { 
          case e: PatternSyntaxException =>
            throw new InvalidArgumentException(s"\n${e.getMessage}")
        }
      }
      
      banner = s"usage: $scriptName $name [<options>] [ <path> | <url> [<path...] ]"
      separator("")
      separator(description)
      separator("Options:")
    
      reqd[Int]("-l", "--limit=<number>", "Limit the number of commits displayed") {
        (value, options) => options.copy(limit = Some(value))
      }
    
      int("-<number>", "Limit the number of commits displayed (shorthand for -l)") {
        (value, options) => options.copy(limit = Some(value))
      }
    
      flag("-a", "--author", "Display the author of each commit")
          { _.copy(author = true) }
        
      flag("-d", "--date", "Display the date of each commit")
          { _.copy(date = true) }
    
      flag("-t", "--time", "Display the date and time of each commit")
          { _.copy(date = true, time = true) }
    
      flag("-f", "--full", "Display the full commit messages")
          { _.copy(full = true) }
    
      flag("-p", "--paths", "Display the paths affected by each commit")
          { _.copy(showPaths = true) }
    
      flag("-v", "--verbose", "Shorthand for --author --time --full")
          { _.copy(author = true, date = true, time = true, full = true) }
    
      reqd[String]("-r", "--revision=<revision>", "Specify a revision or a range of revisions")
        { (revision, options) => options.copy(revisions = options.revisions :+ revision) }

      flag("", "--reverse", "Output the chosen commits in the reverse order")
        { _.copy(reverse = true) }
      
      flag("-i", "--incoming", "Display commits incoming with next update")
          { options => options.copy(incoming = true, revisions = options.revisions :+ "HEAD:BASE") }

      flag("", "--stop-on-copy", "Do not cross copies while traversing history")
        { _.copy(noCopy = true) }
      
      reqd[Regex]("-m", "--match=<regex>", "Limits commits to those with a message containing <regex>")
        { (regex, options) => options.copy(regexes = options.regexes :+ regex) }
        
      reqd[String]("-s", "--search=<glob>", "Limits commits to those with a message containing <glob>")
        { (glob, options) => options.copy(search = options.search :+ SearchOption(glob, searchAnd = false)) }
      
      reqd[String]("", "--search-and=<glob>", "Combine <glob> with the previous search pattern")
        { (glob, options) => options.copy(search = options.search :+ SearchOption(glob, searchAnd = true)) }

      flag("-h", "--help", "Show this message")
          { _ => println(help); throw HelpException() }
  
      arg[String] { (path, options) => options.copy(paths = options.paths :+ path) }
    
      separator("")
      separator("By default shows only the first line of each commit message (see --full)")
      separator("If only 1 revision is given and it is not a range then :0 is appended to make it a range.")
      separator("If no revision is given and the first path looks like a revision it will be treated as one")
    }
  
    parser.parse(args, Options())
  }
  
  def showResults(options: Options): Unit = {
    import scala.xml._
    
    //  Filter the log entries to those that match any regular expressions
    //  supplied by the user
    def matching(entries: Seq[LogEntry]): Seq[LogEntry] =
      if (options.regexes.isEmpty)
        entries
      else entries filter { entry =>
        val msg = entry.msg.mkString("\n")
        options.regexes exists (_.contains(msg))
      }
        
    val entries = matching(getLogEntries(options))

    // When showing incoming changes there is a possiblity that the BASE
    // revision already exists in the working directory.
    // If --incoming was used with no other --revision arguments,
    // we check for this an do not display the local commit.
    // We assume the path given is a working copy path because using -rBASE
    // for a URL would have caused an error when we ran the log command.
    val omitRev = if (entries.nonEmpty && options.incoming && options.revisions.size == 1) {
      def parentDir(path: String) = {
        val withDir = """^(.*)/[^/]+""".r
        path.chomp("/") match {
          case withDir(dir) => dir
          case _            => "."
        }
      }
      val wcPath = options.paths.headOption getOrElse "."
      val wcPathInfo = svn.info(wcPath)
      if (wcPathInfo.kind == "dir")
        Some(wcPathInfo.commitRev)
      else
        Some(svn.info(parentDir(wcPath)).commitRev)
    }
    else
      None
    
    //  First get the length of the longest revision string and author name
    val (maxRevLen, maxAuthorLen) = if (options.author)
      entries.foldLeft((0, 0)) {
        case ((maxRev, maxAuthor), entry) =>
          (entry.revision.length max maxRev, entry.author.length max maxAuthor)
      }
    else
      (entries.foldLeft(0) { (maxRev, entry) => entry.revision.length max maxRev }, 0)

    def buildPrefix(revision: String, author: String, date: LocalDateTime): String = {
        val revFormat    = s"%${maxRevLen}s"
        val authorFormat = s"%-${maxAuthorLen}s"
        val dateStr      = if (options.time) displayDateTime(date) else displayDate(date)
        
        (options.author, options.date) match {
          case (true, true)  => s"${yellow(revFormat)} ${cyan(authorFormat)} ${purple("%s")}".format(revision, author, dateStr)
          case (true, false) => s"${yellow(revFormat)} ${cyan(authorFormat)}".format(revision, author)
          case (false, true) => s"${yellow(revFormat)} ${purple("%s")}".format(revision, dateStr)
          case _             => s"${yellow(revFormat)}".format(revision)
        }
    }
    
    val orderedEntries = if (options.reverse) entries.reverse else entries
    for (LogEntry(revision, author, date, fullMsg, logPaths) <-orderedEntries if Some(revision) != omitRev) {
      val msg1st   = fullMsg.headOption getOrElse ""
      val prefix   = buildPrefix(revision, author, date)

      if (options.full) {
        println(prefix)
        fullMsg foreach (line => println(s"  $line"))
      }
      else
        println(s"${prefix} ${msg1st}")
      
      if (options.showPaths)
        logPaths foreach (p => println(formattedLogPath(p)))
    }
  }

  //  We allow revisions like:
  //    HEAD, 33, 878
  //    HEAD:33    (range)
  //    HEAD-3     (three revisions before HEAD)
  //    556+2      (two revisions after 556)
  val REV = """^(\d+|HEAD|BASE|PREV|COMMITTED)(?::(\d+|HEAD|BASE|PREV|COMMITTED)|([+-])(\d+))?$$""".r
    
  private def looksLikeRevision(str: String): Boolean = { REV matches str }

  private def resolveRevision(revString: String, path: String): String = {
    revString match {
      case REV(rev, null, null, null) =>
        revString
      case REV(rev1, rev2, null, null) =>
        revString
      case REV(rev, null, op, delta) =>
        val revs  = Seq(if (op == "-") s"$rev:0" else s"$rev:HEAD")
        val limit = Some(delta.toInt + 1)
        val entries = svn.log(paths = Seq(path), revisions = revs, limit = limit, includeMessage = false)
        entries.last.revision
      case _ =>
        generalError(s"Cannot resolve revision from $revString for path ($path)")
    }
  }
  
  
  def getLogEntries(options: Options): Seq[LogEntry] = {
    //  If no revisions are specified and the first 'path' looks like a revision
    //  then treat it as one, appending :0 if it does not have a range.
    val (paths, revisions) = if (options.revisions.isEmpty && options.paths.nonEmpty && looksLikeRevision(options.paths.head))
    (options.paths.tail, Vector(options.paths.head))
    else
      (options.paths, options.revisions)
    val resolvedRevs = revisions map { r => resolveRevision(r, paths.headOption getOrElse ".") }
    //  If only a single revision is specified and it does not contain
    //  a range,then we add :0 so that we get the log starting from that point.
    val finalRevs = if (resolvedRevs.size == 1 && !(resolvedRevs.head contains ':'))
        Vector(s"${resolvedRevs.head}:0")
    else
      resolvedRevs
    
    svn.log(
      paths         = paths,
      revisions     = finalRevs,
      limit         = options.limit,
      includePaths  = options.showPaths,
      stopOnCopy    = options.noCopy,
      searchOptions = options.search
    )
  }
  
  
  override def run(args: Seq[String]): Unit = {
    showResults(processCommandLine(args))    
  } 
}
