
import scala.sys.process.{ Process, ProcessBuilder, ProcessLogger }
import java.io.{ File, IOException }
import scala.collection.mutable.ListBuffer
import sbt._


object Utilities {
  
  
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

  def findRootCause(e: Throwable): Throwable =
    if (e.getCause == null || e.getCause == e) return e else return findRootCause(e.getCause)

  //  Execute a console command, returning the return status,
  //  stdout and stdin.
  //  The first commponent of the command is the name of the command,
  //  and the rest are arguments passed on the command line.
  def exec(command: Seq[String], cwd: Option[File] = None): (Int, Seq[String], Seq[String]) = {
    val logger = new ExecLogger
    try {
      val status = Process(command, cwd) ! logger
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
  
  
  def createZipFile(zipfile: File, cwd: File, entries: Traversable[File]): Int = {
    val command = Seq("zip", zipfile.getPath) ++ (entries.toSeq map (_.getPath))
    val (status, stdout, stderr) = exec(command, Some(cwd))
    if (status != 0)
      (stdout ++ stderr) foreach println
    status
  }

}
