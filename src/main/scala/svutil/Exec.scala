
package svutil

import scala.sys.process.{ Process, ProcessBuilder, ProcessLogger }
import java.io.{ File, IOException }
import scala.collection.mutable.ListBuffer


object Exec {

  case class ExecError(err: Int, stderr: Seq[String]) extends Exception("Exec Error")
  
  class ConsoleExecLogger extends ProcessLogger {
    def buffer[T](f: => T): T = f
    def err(s: => String): Unit = System.err.println(s)
    def out(s: => String): Unit = System.out.println(s)
  }
  
  def findRootCause(e: Throwable): Throwable =
    if (e.getCause == null || e.getCause == e) return e else return findRootCause(e.getCause)

  //  The first commponent of the command is the name of the command,
  //  and the rest are arguments passed on the command line.
  def exec(command: Seq[String], cwd: Option[File], logger: ProcessLogger): Int = {
    try { Process(command, cwd) ! logger }
    catch {
      case t: Throwable => findRootCause(t) match {
        case e: IOException if e.getMessage != null => """error=(\d+)""".r.findFirstMatchIn(e.getMessage) match {
          case Some(m) =>
            logger.err(e.getMessage)
            m.group(1).toInt
            
          case _ =>
            logger.err(e.getMessage)
           -1
        }
        
        case e: IOException =>
          -1
      }
    }
  }
    
  //  Execute a console command, returning the return status, stdout and stdin.
  //  The first commponent of the command is the name of the command,
  //  and the rest are arguments passed on the command line.
  def simpleExec(command: Seq[String], cwd: Option[File] = None): (Int, Seq[String], Seq[String]) = {
    class SimpleExecLogger extends ProcessLogger {
      val outbuf = new ListBuffer[String]
      val errbuf = new ListBuffer[String]
      def stdout: Seq[String] = outbuf.toList
      def stderr: Seq[String] = errbuf.toList
      def buffer[T](f: => T): T = f
      def err(s: => String): Unit = errbuf += s
      def out(s: => String): Unit = outbuf += s
    }
    
    val logger = new SimpleExecLogger
    val status = exec(command, cwd, logger)
    
    (status, logger.stdout, logger.stderr)
  }
  
  //  Execute the command and return the commands stdout
  //  If the command fails, throw an ExecError
  def runCmd(command: Seq[String], cwd: Option[File] = None): Seq[String] = {
    simpleExec(command, cwd) match {
      case (0, stdout, _)   => stdout
      case (err, _, stderr) => throw ExecError(err, stderr)
    }
  }
}

