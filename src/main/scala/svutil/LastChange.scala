

package svutil

import java.io.File
import scala.util.matching.Regex
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
    val fileInfo = getSvnInfo(options.path, Some(options.rev))
    
    if (fileInfo.kind != "file")
      generalError(s"${options.path} does not reference a file entry in the repository")
    
    println(blue(fileInfo.url))
    println(s"Last change since: ${yellow(fileInfo.repoRev)}")
    println("-------------------------------------------------------------------")
    println(s"Commit: ${yellow(fileInfo.commitRev)}")
    println(s"Author: ${cyan(fileInfo.commitAuthor)}")
    println(s"Date  : ${purple(displayDateTime(fileInfo.commitDate))}")
    
    if (options.diff)
      showChangeDiff(fileInfo.url, fileInfo.commitRev)
  } 
}
