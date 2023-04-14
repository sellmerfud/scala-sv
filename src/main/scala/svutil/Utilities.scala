
package svutil

import java.util.Locale
import java.time.format.DateTimeFormatter
import scala.util.Properties.propOrElse
import java.time._
import java.io.File
import scala.xml._
import Color._
import svn.model.{ LogEntry }

object Utilities {
  
  lazy val scriptPath = propOrElse("sv.scriptname", "sv")
  lazy val scriptName = new File(scriptPath).getName
  
  case class GeneralError(message: String = "") extends Exception(message)
  case class HelpException() extends Exception("Help Exception")
  case class SuccessExit(message: String = "") extends Exception(message)
  
  def generalError(msg: String): Nothing = throw GeneralError(msg)
  def successExit(msg: String): Nothing = throw SuccessExit(msg)
  
  implicit class StringWrapper(str: String) {
    def chomp(suffix: String = "\n"): String = suffix match {
      case null | "" => str
      case "\n" if str.endsWith("\r\n")                     => str.slice(0, str.length - 2)
      case "\n" if str.endsWith("\r") || str.endsWith("\n") => str.slice(0, str.length - 1)
      case suf  if str.endsWith(suf)                        => str.slice(0, str.length - suf.length)
      case _                                                => str
    }
    def isInteger = str forall (_.isDigit)
  }

  // Join paths returned by svn commands
  def joinPaths(base: String, others: String*): String = {
    val result = new StringBuilder(base.chomp("/"))
    for (segment <- others)
      result.append("/").append(segment.chomp("/"))
    
    result.result()
  }
  
  val ISODateFormat     = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.nX")
  val DisplayDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  val DisplayTimeFormat = DateTimeFormatter.ofPattern("HH:mm:ss")
  
  //  The svn XML output returns dates in ISO format in the UTC time zone
  //  We parse them and convert them to local date and time
  def parseISODate(isoDate: String): LocalDateTime = {
    val utc = ZonedDateTime.parse(isoDate, ISODateFormat)
    utc.withZoneSameInstant(ZoneId.systemDefault).toLocalDateTime
  }
  
  //  Convert the local date to a utc and format it as a string
  def toISODateString(date: LocalDateTime): String = {
    val zonedDate = date.atZone(ZoneId.systemDefault)
    val utcDate   = zonedDate.withZoneSameInstant(ZoneId.ofOffset("", ZoneOffset.UTC))    
    utcDate.format(ISODateFormat)
  }
  
  def displayDate(date: LocalDateTime): String = date.format(DisplayDateFormat)
  def displayTime(date: LocalDateTime): String = date.format(DisplayTimeFormat)
  def displayDateTime(date: LocalDateTime): String = s"${displayDate(date)} ${displayTime(date)}"

  //  We create a .sv directory in the top directory of the working copy
  //  This gives sv commands a place to store data
  //  This will throw an error of the directory cannot be resloved.
  def getDataDirectory(): os.Path = {
    val wcRoot  = svn.workingCopyRoot getOrElse {
      generalError(s"You must run this command from within a subversion working copy directory")
    }
    
    try {
      val dotSVDir = wcRoot / ".sv"
      if (!os.isDir(dotSVDir)) {
        try os.makeDir(dotSVDir)
        catch {
          case e: java.io.IOException =>
            generalError(s"Cannot create .sv directory: ${e.getMessage}")
        }
      }
      dotSVDir
    }
    catch {
      case e: java.io.IOException =>
        val msg = if (e.getMessage == null) "" else s": ${e.getMessage}"
        generalError(s"Unable to create the .sv directory in your working copy$msg")
    }
  }
  
  def amtOf(num: Int, singular: String) = num match {
    case 1 => s"$num $singular"
    case _ => s"$num ${singular}s"
  }
  
  
  def showCommit(logEntry: LogEntry, showMsg: Boolean = true, showPaths: Boolean = true): Unit = {
    println("-------------------------------------------------------------------")
    println(s"Commit: ${yellow(logEntry.revision)}")
    println(s"Author: ${cyan(logEntry.author)}")
    println(s"Date  : ${purple(displayDateTime(logEntry.date))}")
    println("-------------------------------------------------------------------")

    if (showMsg)
      logEntry.msg foreach (m => println(s"  $m"))
  
    println()
    //  Path summary
    if (logEntry.paths.nonEmpty) {
      case class Totals(mod: Int = 0, add: Int = 0, del: Int = 0)
      val totals = logEntry.paths.foldLeft(Totals()) { (t, p) =>
        p.action match {
          case "M" => t.copy(mod = t.mod + 1)
          case "A" => t.copy(add = t.add + 1)
          case "D" => t.copy(del = t.del + 1)
          case _   => t
        }
      }
      println(cyan(s"${amtOf(totals.mod, "file")} modified, ${totals.add} added, ${totals.del} deleted"))
    }
    if (showPaths)
      logEntry.paths foreach (p => println(p.formatted))
  }
  
  def printDiffLine(line: String): Unit = {
    val color: (String) => String = {
      if      ((line startsWith "---") || 
               (line startsWith "+++"))                blue
      else if (line startsWith "Index:")               yellow
      else if (line startsWith "==========")           yellow
      else if (line startsWith "Property changes on:") purple
      else if (line startsWith "+")                    green
      else if (line startsWith "@@")                   gray
      else if (line startsWith "-")                    red
      else                                             white
    }

    println(color(line))
  }
  
  
  case class ExecError(err: Int, stderr: Seq[String]) extends Exception("Exec Error")

  implicit class CommandResultWrapper(r: os.CommandResult) {
      def outString = new String(r.out.bytes)
      def errString = new String(r.err.bytes)
      def outSeq = outString.split("\n").toSeq
      def errSeq = errString.split("\n").toSeq
  }
  
  //  Execute the command and return the commands stdout
  //  If the command fails, throw an ExecError
  def runCmd(command: Seq[String], cwd: Option[os.Path] = None): Seq[String] = {
    import os.Shellable._
    val r = os.proc(command).call(cwd = cwd getOrElse null, check = false, stderr = os.Pipe)
    if (r.exitCode == 0)
      r.outSeq
    else
      throw ExecError(r.exitCode, r.errSeq)
  }
  
}


