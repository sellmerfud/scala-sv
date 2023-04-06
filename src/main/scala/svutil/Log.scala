

package svutil

import scala.util.matching.Regex
import java.time.LocalDateTime
import Color._
import Exec.runCmd
import Utilities._

object Log extends Command {
  
  override val name = "log"
  override val description = "Display formatted log entries"
  case class Search(glob: String, searchAnd: Boolean)
  case class Options(
    limit:     Option[Int]    = None,
    author:    Boolean        = false,
    date:      Boolean        = false,
    time:      Boolean        = false,
    full:      Boolean        = false,
    showPaths: Boolean        = false,
    show:      Boolean        = false,
    noCopy:    Boolean        = false,
    incoming:  Boolean        = false,
    reverse:   Boolean        = false,
    revisions: Vector[String] = Vector.empty,
    search:    Vector[Search] = Vector.empty,
    paths:     Vector[String] = Vector.empty)


  private def processCommandLine(args: Seq[String]): Options = {
    import org.sellmerfud.optparse._
  
    val parser = new OptionParser[Options] {
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
    
      reqd[String]("-r", "--revision=<revision>", "Specify a revision or a range of revisions",
                                                  "Can be specified multiple times",
                                                  "See the svn log help for more information")
        { (revision, options) => options.copy(revisions = options.revisions :+ revision) }
      
      flag("", "--reverse", "Output the chosen commits in the reverse order")
        { _.copy(reverse = true) }
      
      flag("-i", "--incoming", "Display commits incoming with next update", "shorthand for -rHEAD:BASE")
          { options => options.copy(incoming = true, revisions = options.revisions :+ "HEAD:BASE") }

      flag("", "--stop-on-copy", "Do not cross copies while traversing history")
        { _.copy(noCopy = true) }
      
      reqd[String]("-s", "--search=<glob>", "Limits commits to those with a message containing <glob>")
        { (glob, options) => options.copy(search = options.search :+ Search(glob, searchAnd = false)) }
      
      reqd[String]("", "--search-and=<glob>", "Combine <glob> with the previous search pattern")
        { (glob, options) => options.copy(search = options.search :+ Search(glob, searchAnd = true)) }

      flag("", "--show", "Show the resulting svn log command instead of running it")
        { _.copy(show = true) }
      
      flag("-h", "--help", "Show this message")
          { _ => println(help); throw HelpException() }
  
      arg[String] { (path, options) => options.copy(paths = options.paths :+ path) }
    
      separator("")
      separator("By default shows only the first line of each commit message (see --full)")
    }
  
    parser.parse(args, Options())
  }
  
  def showResults(cmdLine: Seq[String], options: Options): Unit = {
    import scala.xml._
    
    val logData = runCmd(cmdLine).mkString("\n")
    // Parse the XML log entries
    val entries = (XML.loadString(logData) \ "logentry") map parseLogEntry

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
      val wcPathInfo = getSvnInfo(wcPath)
      if (wcPathInfo.kind == "dir")
        Some(wcPathInfo.commitRev)
      else
        Some(getSvnInfo(parentDir(wcPath)).commitRev)
    }
    else
      None
    
    //  First get the length of the longest revision string
    val maxRevLen = entries.foldLeft(0) { (maxLen, entry) => entry.revision.length max maxLen }
    val maxAuthorLen = if (options.author)
      entries.foldLeft(0) { (maxLen, entry) => entry.author.length max maxLen }      
    else
      0

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
        logPaths foreach (p => println(p.formatted))
    }
  }
    
  def looksLikeRevision(str: String): Boolean = {
   val revPart = """(?:\d+|HEAD|BASE|PREV|COMMITTED)""".r
   s"""^$revPart(?::$revPart)?$$""".r matches str 
  }
    
  def buildCmdLine(options: Options): Seq[String] = {
    var cmdLine = Vector[String]("svn", "log", "--xml")
    
    options.limit foreach { limit => cmdLine :+= s"--limit=${limit}" } 
    if (options.showPaths)
      cmdLine :+= "--verbose"
    if (options.noCopy)
      cmdLine :+= "--stop-on-copy"
    
    cmdLine :++= (options.search map { 
      case Search(glob, false) => s"--search=$glob"
      case Search(glob, true) => s"--search-and=$glob"
    })
    
    //  If no revisions are specified and the first 'path' looks like a revision
    //  then treat it as one, appending :0 if it does not have a range.
    //
    //  If only a single revision is specified and it does not contain
    //  a range,then we add :0 so that we get the log starting from that point.
    val paths = if (options.revisions.isEmpty && options.paths.nonEmpty && looksLikeRevision(options.paths.head)) {
      val rev = options.paths.head
      val fullRev = if (rev contains ':') rev else s"$rev:0"
      cmdLine :+= s"--revision=${fullRev}"
      options.paths.tail
    }
    else {
      if (options.revisions.size == 1 && !(options.revisions.head contains ':'))
        cmdLine :+= s"--revision=${options.revisions.head}:0"
      else
        cmdLine :++= (options.revisions map { revision => s"--revision=$revision" })
      options.paths
    }  
      
    cmdLine :++= paths          
    cmdLine
  }
  
  
  override def run(args: Seq[String]): Unit = {
    val options = processCommandLine(args)
    val cmdLine = buildCmdLine(options)
    val status = if (options.show)
      println(cmdLine mkString " ")
    else
      showResults(cmdLine, options)    
  } 
}
