

package svutil

import java.io.File
import scala.util.matching.Regex
import scala.util.Properties.propOrElse
import scala.xml._
import Exec.{ runCmd, ExecError }
import Color._
import svutil.exceptions._
import Utilities._

object Ignore extends Command {
  
  override val name = "ignore"
  override val description = "Write ignore properties to stdout in .gitignore format"
  
  case class Options(path: String = ".")
  
  private def processCommandLine(args: Seq[String]): Options = {
    import org.sellmerfud.optparse._
    
    val scriptName = propOrElse("svutil.script.name", "sv")
    
    val parser = new OptionParser[Options] {
      banner = s"usage: $scriptName $name [<options>] [<path>]"
      
      flag("-h", "--help", "Show this message")
          { _ => println(help); sys.exit(0) }
    
      arg[String] { (path, options) => options.copy(path = path.chomp("/")) }  
        
      separator("")
      separator("<path> must refer to a working directory and not a repository url.")
      separator("If <path> is omitted the current directory is used by default.")
    }
    
    parser.parse(args, Options())
  }
  
    
  private def isDir(path: String) = new File(path).isDirectory
  
  // Make sure it refers to a directory in the working copy
  private def isWorkingCopyDirectory(path: String): Boolean = {
    val info = getSvnInfo(path)
    info.workingCopyPath.nonEmpty && info.kind == "dir"
  }
  
  private def displayIgnoreEntries(options: Options): Unit = {
    val prefixLen = options.path.length + 1  // Add one for the trailing slash  
    def svnIgnore(dirPath: String): Unit = {
      val ignores = try runCmd(Seq("svn", "pget", "svn:ignore", dirPath)) map (_.trim) filter (_ != "")
      catch {
        case ExecError(_, _) =>  Seq.empty  // No svn:ignore defined for this url
      }
      
      for (ignore <- ignores) {
        val ignorePath = joinPaths(dirPath, ignore.chomp("/"))
        val rebasedPath = ignorePath drop prefixLen
        // We prefix each path with a slash so that it refers to the
        // specific entry as per .gitignore rules.
        // See: https://git-scm.com/docs/gitignore
        if (isDir(ignorePath))
          println(s"/$rebasedPath/")
        else
          println(s"/$rebasedPath")
      }
      
      //  Recursively process all subdirectories
      val subDirs = getSvnLists(dirPath).head.entries filter (_.kind =="dir")
      for (subdir <- subDirs) {
          val subDirPath = joinPaths(dirPath, subdir.name.chomp("/"))
          svnIgnore(subDirPath)
      }
    }
    
    
    if (!isWorkingCopyDirectory(options.path))
      throw GeneralError(s"${options.path} is not a subversion working copy directory")

    svnIgnore(options.path)
  }
  
  override def run(args: Seq[String]): Unit = {
    val options = processCommandLine(args)
    displayIgnoreEntries(options)
  } 
}
