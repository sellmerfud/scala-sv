
package svutil

import java.io.{ IOException, BufferedReader, InputStreamReader }
import scala.util.Properties.propOrElse

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
    
    Option(classLoader.getResourceAsStream("version")) match {
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
  
  lazy val commands = Log :: Branch :: Nil
  
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
  
  val STATUS_OK          = 0
  val STATUS_INVALID_CMD = 1
    
    
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
          println(s"$scriptName: '$cmdName' is not a $scriptName command.  See '$scriptName --help'.")
          STATUS_INVALID_CMD
          
        case Some(command) =>
          command.run(cmdArgs.tail)
      }
    }
    
    sys.exit(status)
  } 
}
