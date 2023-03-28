

package svutil

import scala.util.matching.Regex
import java.time.LocalDateTime
import Color._
import Exec.runCmd
import svutil.exceptions._
import Utilities._

object Log extends Command {
  
  override val name = "log"
  override val description = "Display formatted subversion log messages"
  
  case class Options(
    author: Boolean = false,
    date:   Boolean = false,
    time:   Boolean = false,
    full:   Boolean = false,
    paths:  Boolean = false,
    show:   Boolean = false)


  def showResults(logData: String, options: Options): Unit = {
    import scala.xml._
    
    // Parse the XML log entries
    val entries = (XML.loadString(logData) \ "logentry") map parseLogEntry
    
    //  First get the length of the longest revision string
    val maxRevLen = entries.foldLeft(0) { (maxLen, entry) => entry.revision.length max maxLen }
    val maxAuthorLen = if (options.author)
      entries.foldLeft(0) { (maxLen, entry) => entry.author.length max maxLen }      
    else
      0

    def buildPrefix(revision: String, author: String, date: LocalDateTime): String = {
        val revFormat    = s"%-${maxRevLen}s"
        val authorFormat = s"%-${maxAuthorLen}s"
        val dateStr      = if (options.time) displayDateTime(date) else displayDate(date)
        
        (options.author, options.date) match {
          case (true, true)  => s"${yellow(revFormat)} ${cyan(authorFormat)} ${purple("%s")}".format(revision, author, dateStr)
          case (true, false) => s"${yellow(revFormat)} ${cyan(authorFormat)}".format(revision, author)
          case (false, true) => s"${yellow(revFormat)} ${purple("%s")}".format(revision, dateStr)
          case _             => s"${yellow(revFormat)}".format(revision)
        }
    }
    
    for (LogEntry(revision, author, date, fullMsg, logPaths) <- entries) {
      val msg1st   = fullMsg.headOption getOrElse ""
      val prefix   = buildPrefix(revision, author, date)

      if (options.full) {
        println(prefix)
        fullMsg foreach (line => println(s"  $line"))
      }
      else
        println(s"${prefix} ${msg1st}")
      
      if (options.paths) {
        for (LogPath(path, kind, action, textMods, propMods, fromPath) <- logPaths) {
          val coloredAction = action match {
            case "D" => red("D")
            case "A" => green("A")
            case _   => white(action)
          }
          val from = fromPath match {
            case Some(FromPath(path, revision)) => s"  (from ${path}:${revision})"
            case None                           => ""
          }
          println(s"  ${coloredAction} ${blue(path)}${red(from)}")
        }
      }
    }
  }
  
  
  def showUsage(): Unit = {
    val usage = s"""|usage: $scriptName $name [<options>] [<path>]
                    |  -{number}        : limits the number of log entries to {number}
                    |  -a, --author     : displays the user who made the commit
                    |  -d, --date       : displays the date for each commit
                    |  -t, --time       : displays the date and time for each commit
                    |  -f, --full       : displays the full commit message
                    |  -p, --paths      : displays the affected paths
                    |  -i, --incoming   : displays commits incoming with next update
                    |  -v, --verbose    : same as --author --time --full
                    |  -h, --help       : display usage and exit.
                    |  <path>           : default is the current directory""".stripMargin
    println(usage)
    println("\nAll other arguments are passed through to the svn log command")
  }
  
  
  def processCommandLine(args: Seq[String]): (Seq[String], Options) = {
    var cmdLine = Vector[String]("svn", "log")
    var options = Options()
    val ignoredLong = Set("--quiet", "--diff", "--incremental", "--use-merge-history", "--with-no-revprops", "--force-interactive")
    val ignoredShort = Set('q', 'g')

    def singleDash(arg: String) = arg.startsWith("-") && !arg.startsWith("--")
    def singleLettersArgs(letters: String): Unit = if (letters.length > 0) {
      import Character.isDigit
      def nextChar = singleLettersArgs(letters drop 1)
        letters(0) match {
          case 'h'                    => showUsage(); throw HelpException()
          case 'a'                    => options = options.copy(author = true); nextChar
          case 'd'                    => options = options.copy(date = true); nextChar
          case 't'                    => options = options.copy(date = true, time = true); nextChar
          case 'f'                    => options = options.copy(full = true); nextChar
          case 'p'                    => options = options.copy(paths = true); nextChar
          case 'i'                    => cmdLine :+= "-rHEAD:BASE"; nextChar
          case 'v'                    => options = options.copy(author = true, date = true, time = true, full = true); nextChar
          case 'r' | 'c' | 'l' | 'x'  => cmdLine :+= s"-$letters" // These are followed by an argument...
          case ch if ignoredShort(ch) => nextChar
          case ch if isDigit(ch)      =>
            cmdLine :+= s"-l${letters takeWhile isDigit}"
            singleLettersArgs(letters dropWhile isDigit)
          case ch                     => cmdLine :+= s"-$ch"; nextChar
        }
        
    }
  
    args foreach {
      case "--help"                => showUsage(); throw HelpException()
      case "--quiet"               => // Ignore these svn args as they do not apply
      case "--author"              => options = options.copy(author = true)
      case "--date"                => options = options.copy(date = true)
      case "--time"                => options = options.copy(date = true, time = true)
      case "--full"                => options = options.copy(full = true)
      case "--incoming"            => cmdLine :+= "-rHEAD:BASE"
      case "--paths"               => options = options.copy(paths = true)
      case "--verbose"             => options = options.copy(author = true, date = true, time = true, full = true)
      case "--show"                => options = options.copy(show = true)
      case arg if ignoredLong(arg) =>
      case arg if singleDash(arg)  => singleLettersArgs(arg drop 1)
      case arg                     => cmdLine :+= arg
    }
    
    if (options.paths)
      cmdLine :+= "--verbose"
    
    cmdLine :++= Vector("--xml", "--with-all-revprops", "--non-interactive")
    
    (cmdLine, options)
  }
  
  
  override def run(args: Seq[String]): Unit = {
    val (cmdLine, options) = processCommandLine(args)
    val status = if (options.show)
      println(cmdLine mkString " ")
    else
      showResults(runCmd(cmdLine).mkString("\n"), options)    
  } 
}
