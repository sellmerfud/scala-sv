
package svutil

import java.io.{ IOException, BufferedReader, InputStreamReader }
import scala.util.Properties.propOrElse
import svutil.exceptions._
import Exec.ExecError
import org.sellmerfud.optparse.OptionParserException

object Main {

  lazy val SOFTWARE_VERSION: String = {
    def classLoader: ClassLoader =
      Thread.currentThread.getContextClassLoader match {
        case null => this.getClass.getClassLoader match {
          case null => ClassLoader.getSystemClassLoader
          case cl => cl
        }
        case cl => cl
      }
    
    Option(classLoader.getResourceAsStream("svutil/version")) match {
      case None           => "Missing"
      case Some(resource) =>
        try {
          val reader = new BufferedReader(new InputStreamReader(resource))
          val version = reader.readLine.trim
          reader.close
          version
        }
        catch {
          case e: IOException => "Error"
        }
    }
  }
  
  lazy val commands = Log :: Branch :: LastChange :: FileRevs :: Ignore :: Nil
  
  def showHelp(scriptName: String): Unit = {
    val usage = s"""|usage: $scriptName [-v | --version]
                    |       $scriptName [-h | --help | help]
                    |       $scriptName <command> [<args>]
                    |
                    |The available commands are:
                    |""".stripMargin
                   
      println(usage)
      for (c <- commands)
        println(f"  ${c.name}%-8s  ${c.description}")
      println(s"\nFor help about a particular command use $scriptName <command> --help")
  }
  
  val STATUS_OK  = 0
  val STATUS_BAD = 1
    
    
  def main(args: Array[String]): Unit = {
    val scriptName = propOrElse("svutil.script.name", "sv")
    val mainOpts   = args.toSeq takeWhile (_ startsWith "-")
    val cmdArgs    = args.toSeq dropWhile (_ startsWith "-")
    val cmdName    = cmdArgs.headOption getOrElse "help"
    
    val status = if ((mainOpts contains "--version") || (mainOpts contains "-v")) {
      println(s"Subversion Utilities version $SOFTWARE_VERSION")
      STATUS_OK
    }
    else if ((mainOpts contains "--help") || (mainOpts contains "-h") || (cmdName == "help")) {
      showHelp(scriptName)
      STATUS_OK
    }
    else {
      commands.find(_.name == cmdName) match {
        case None =>
          System.err.println(s"$scriptName: '$cmdName' is not a $scriptName command.  See '$scriptName --help'.")
          STATUS_BAD
          
        case Some(command) =>
          try {
            command.run(cmdArgs.tail)
            STATUS_OK
          }
          catch {
            case ExecError(errStatus, stderr) =>
              stderr foreach System.err.println
              errStatus
              
            case e: OptionParserException =>
              System.err.println(e.getMessage)
              STATUS_BAD
              
            case HelpException() =>
              // User used --help option for the command
              STATUS_OK
              
            case GeneralError(reason) =>
              if (reason != "")
                System.err.println(reason)
              STATUS_BAD
              
            case e: Throwable =>
              System.err.println(Option(e.getMessage) getOrElse e.getClass.getName)
              STATUS_BAD
          }
      }
    }
    
    sys.exit(status)
  } 
}
