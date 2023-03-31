
package svutil

import java.io.{ IOException, BufferedReader, InputStreamReader }
import scala.util.{ Try, Success, Failure }
import svutil.exceptions._
import Exec.ExecError
import Utilities._
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
  
  lazy val commands = Log::Branch::Show::FileRevs::Bisect::Ignore :: Nil
  
  def showHelp(): Unit = {
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
      println(s"Commands may be abbreviated to their shortest unique prefix")
  }
  
  def matchCommand(name: String): List[Command] = {
    if ("""^[a-zA-Z][-a-zA-Z0-9_]*""".r matches name)
      commands filter (_.name startsWith name)
    else
      Nil
  }
  
  val STATUS_OK  = 0
  val STATUS_BAD = 1
    
    
  def main(args: Array[String]): Unit = {
    val mainOpts   = args.toSeq takeWhile (_ startsWith "-")
    val cmdArgs    = args.toSeq dropWhile (_ startsWith "-")
    val cmdName    = cmdArgs.headOption getOrElse "help"
    
    val status = if ((mainOpts contains "--version") || (mainOpts contains "-v")) {
      println(s"Subversion Utilities version $SOFTWARE_VERSION")
      STATUS_OK
    }
    else if ((mainOpts contains "--help") || (mainOpts contains "-h")) {
      showHelp()
      STATUS_OK
    }
    else if (cmdName == "help") {
      (cmdArgs.drop(1).headOption map matchCommand) match {
          case Some(cmd::Nil) => Try(cmd.run(Seq("--help")))
          case _ =>showHelp()
      }
      STATUS_OK
    }
    else {
      matchCommand(cmdName) match {
        case command :: Nil =>
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

        case Nil =>
          System.err.println(s"$scriptName: '$cmdName' is not a $scriptName command.  See '$scriptName --help'.")
          STATUS_BAD
          
        case commands =>
          System.err.println(s"$scriptName: command '$cmdName' is ambiguous.  (${(commands map (_.name)).mkString(", ")})")
          STATUS_BAD
      
      }
    }
    
    sys.exit(status)
  } 
}
