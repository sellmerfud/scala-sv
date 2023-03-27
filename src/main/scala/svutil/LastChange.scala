

package svutil

import java.io.File
import scala.util.matching.Regex
import scala.util.Properties.propOrElse
import scala.xml._
import Exec.{ runCmd, ExecError }
import Color._
import svutil.exceptions._
import Utilities._

object LastChange extends Command {
  
  override val name = "last"
  override val description = "Show the last change of a given file"
  
  case class Options(
    rev: String   = "HEAD",
    path: String  = ".",
    diff: Boolean = false
  )
  
  private def processCommandLine(args: Seq[String]): Options = {
    import org.sellmerfud.optparse._
    
    val scriptName = propOrElse("svutil.script.name", "sv")
    
    val parser = new OptionParser[Options] {
      banner = s"usage: $scriptName $name [<options>] <path>|<url>"
      
      reqd[String]("-r", "--revision=<rev>", "Revision at which to assess the change (default: HEAD)")
        { (rev, options) => options.copy(rev = rev) }
        
      flag("-d", "--diff", "Show the diff of the last change")
        { _.copy(diff = true) }
        
      flag("-h", "--help", "Show this message")
          { _ => println(help); sys.exit(0) }
    
      arg[String] { (path, options) => options.copy(path = path) }  
        
      separator("")
      separator("<path>|<url> must refer to file (not directory)")
      separator("<rev> specifies a revision and can take one of these forms:")
    }
    
    parser.parse(args, Options())
  }
  
    
  case class FileInfo(url: String, relUrl: String, atRev: String, commitRev: String, author: String, date: String)
  
  private def getFileInfo(path: String, rev: String): FileInfo = {
    val out     = runCmd(Seq("svn", "info", "--revision", rev, "--xml", path))
    val entry   = (XML.loadString(out.mkString("\n")) \ "entry").head
    val commit  = (entry \ "commit").head
    val isoDate = (commit \ "date").head.text
    
    if (entry.attributes("kind").head.text != "file")
      throw GeneralError(s"$path does not reference a file entry in the repository")
    
    FileInfo(
      url       = (entry \ "url").head.text,
      relUrl    = (entry \ "relative-url").head.text,
      atRev     = entry.attributes("revision").head.text,
      commitRev = commit.attributes("revision").head.text,
      author    = (commit \ "author").head.text,
      date      = s"${extractISODate(isoDate)} ${extractISOTime(isoDate)}"
    )
  }

  private def showChangeDiff(url: String, commitRev: String): Unit = {
    val out = runCmd(Seq("svn", "diff", "--change", commitRev, url))

    def lineColor(line: String): (String) => String = {
      if      ((line startsWith "---") || (line startsWith "+++")) blue _
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
  
  
  
  override def run(args: Seq[String]): Unit = {
    val options = processCommandLine(args)
    val fileInfo = getFileInfo(options.path, options.rev)
    
    println(blue(fileInfo.url))
    println(s"Last change since: ${yellow(fileInfo.atRev)}")
    println("-------------------------------------------------------------------")
    println(s"Commit: ${yellow(fileInfo.commitRev)}")
    println(s"Author: ${cyan(fileInfo.author)}")
    println(s"Date  : ${purple(fileInfo.date)}")
    
    if (options.diff)
      showChangeDiff(fileInfo.url, fileInfo.commitRev)
  } 
}
