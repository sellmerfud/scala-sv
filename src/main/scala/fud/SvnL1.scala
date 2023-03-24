

package fud

import java.io.IOException
import scala.collection.mutable.ListBuffer
import scala.sys.process.{ Process, ProcessBuilder, ProcessLogger }
import scala.util.matching.Regex


object SvnL1 {
  
  //  By default we use ANSI color escape codes unless we are not connected
  //  to a console or if the user has explicitly forbidden it.
  lazy val withColor = {
    val x = sys.env.getOrElse("SVNL1_COLOR", "yes").toLowerCase.headOption.getOrElse('y')
    System.console != null && "yt1".toSet(x)
  }
      
  val RED    = "\u001b[31m"
  val GREEN  = "\u001b[32m"
  val YELLOW = "\u001b[33m"
  val BLUE   = "\u001b[34m"
  val PURPLE = "\u001b[35m"
  val CYAN   = "\u001b[36m"
  val WHITE  = "\u001b[37m"
  val GREY   = "\u001b[90m"
  val RESET  = "\u001b[0m" 
      
  def colorize(color: String) = if (withColor) color else ""   
  
  lazy val red    = colorize(RED)
  lazy val green  = colorize(GREEN)
  lazy val yellow = colorize(YELLOW)
  lazy val blue   = colorize(BLUE)
  lazy val purple = colorize(PURPLE)
  lazy val cyan   = colorize(CYAN)
  lazy val white  = colorize(WHITE)
  lazy val grey   = colorize(GREY)
  lazy val reset  = colorize(RESET)
  
  
   // Used to capture process output.
  private class ExecLogger extends ProcessLogger {
    private val outbuf = new ListBuffer[String]
    private val errbuf = new ListBuffer[String]
    def stdout: Seq[String] = outbuf.toList
    def stderr: Seq[String] = errbuf.toList
    def buffer[T](f: => T): T = f
    def err(s: => String): Unit = errbuf += s
    def out(s: => String): Unit = outbuf += s
  }
  
  private def findRootCause(e: Throwable): Throwable =
    if (e.getCause == null || e.getCause == e) return e else return findRootCause(e.getCause)

  private def exec(command: Seq[String]): (Int, Seq[String], Seq[String]) = {
    val logger = new ExecLogger
    try {
      val status = Process(command) ! logger
      (status, logger.stdout, logger.stderr)
    }
    catch {
      case t: Throwable => findRootCause(t) match {
        case e: IOException if e.getMessage != null => """error=(\d+)""".r.findFirstMatchIn(e.getMessage) match {
          case Some(m) => (m.group(1).toInt, List(), List(e.getMessage))
          case _ => (-1, List(), List(e.getMessage))
        }
        case e: IOException => (-1, List(), List())
      }
    }
    
  }
  
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
    val entries = XML.loadString(logData) \ "logentry"
    
    //  First get the length of the longest revision string
    val maxRevLen = entries.foldLeft(0) { (maxLen, entry) =>
      val len = entry.attributes("revision").head.text.length
      len max maxLen
    }
    
    val maxAuthorLen = if (options.author) {
      entries.foldLeft(0) { (maxLen, entry) =>
        val len = (entry \ "author").head.text.length
        len max maxLen
      }      
    }
    else
      0
    

    def buildPrefix(revision: String, author: String, date: String, time: String): String = {
        val revFormat    = s"%-${maxRevLen}s"
        val authorFormat = s"%-${maxAuthorLen}s"
        val dateStr      = if (options.time) s"$date $time" else date
        
        (options.author, options.date) match {
          case (true, true)  => s"${yellow}${revFormat}${reset} ${cyan}${authorFormat}${reset} ${purple}%s${reset}".format(revision, author, dateStr)
          case (true, false) => s"${yellow}${revFormat}${reset} ${cyan}${authorFormat}${reset}".format(revision, author)
          case (false, true) => s"${yellow}${revFormat}${reset} ${purple}%s${reset}".format(revision, dateStr)
          case _             => s"${yellow}${revFormat}${reset}".format(revision)
        }
    }
    
    for (entry <- entries) {
      val revision = entry.attributes("revision").head.text
      val author   = (entry \ "author").head.text
      val fullMsg  = (entry \ "msg").head.text.split("\n")
      val msg1st   = fullMsg.headOption getOrElse ""
      //  Get just the date, stripping of the time
      val date     = """^[^T]+""".r.findFirstIn((entry \ "date").head.text) getOrElse ""
      val time     = """(?<=T)[^.]+""".r.findFirstIn((entry \ "date").head.text) getOrElse ""
      val prefix   = buildPrefix(revision, author, date, time)

      if (options.full) {
        println(prefix)
        fullMsg foreach (line => println(s"  $line"))
      }
      else
        println(s"${prefix} ${msg1st}")
      
      if (options.paths) {
        for (pathEntry <- (entry \ "paths" \ "path")) {
          val action = pathEntry.attributes("action").head.text
          val actionColor = action match {
            case "D" => red
            case "A" => green
            case _   => white
          }
          val path     = pathEntry.head.text
          val fromPath = Option(pathEntry.attributes("copyfrom-path")) flatMap (_.headOption) map (_.text) getOrElse ""
          val fromRev  = Option(pathEntry.attributes("copyfrom-rev")) flatMap (_.headOption) map (_.text) getOrElse ""
          val from     = if (fromPath != "") s"  (from ${fromPath}:${fromRev})"
                         else                ""

          println(s"  ${actionColor}${action} ${blue}${path}${red}${from}${reset}")
        }
      }
    }
  }
  
  
  val usageString = """|usage: svnl1 [options] [path...]
                       |  -{number}        : limits the number of log entries to {number}
                       |  -a, --author     : displays the user who made the commit
                       |  -d, --date       : displays the date for each commit
                       |  -t, --time       : displays the date and time for each commit
                       |  -f, --full       : displays the full commit message
                       |  -p, --paths      : displays the affected paths
                       |  -i, --incoming   : displays commits incoming with next update
                       |  -v, --verbose    : same as --author --time --full
                       |      --show       : show the svn log command but do not execute it
                       |  -h, --help       : display usage and exit.""".stripMargin
  
  
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
          case 'h'                    => println(usageString); sys.exit(0)
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
      case "--help"                => println(usageString); sys.exit(0)
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
  
  
  def main(args: Array[String]): Unit = {
  
    val (cmdLine, options) = processCommandLine(args.toSeq)
    val status = if (options.show) {
      println(cmdLine mkString " ")
      0
    }
    else {
      val (status, out, err) = exec(cmdLine)
    
      if (status == 0)
        showResults(out.mkString("\n"), options)
      else
        err foreach println
      status
    }
    
    sys.exit(status)
  } 
}