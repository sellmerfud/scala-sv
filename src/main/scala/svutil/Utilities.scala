
package svutil

import java.util.Locale
import java.time.format.DateTimeFormatter
import java.time._

object Utilities {
  
  implicit class StringWrapper(str: String) {
    def chomp(suffix: String = "\n"): String = suffix match {
      case null | "" => str
      case "\n" if str.endsWith("\r\n") => str.reverse.drop(2).reverse.toString
      case "\n" if str.endsWith("\r") || str.endsWith("\n") => str.reverse.drop(1).reverse.toString
      case suf   if str.endsWith(suf) => str.reverse.drop(suf.length).reverse.toString
      case _ => str
    }
  }
  
  def joinPaths(base: String, others: String*): String = {
    val result = new StringBuilder(base.chomp("/"))
    for (segment <- others)
      result.append("/").append(segment.chomp("/"))
    
    result.result()
  }
  
  val ISODateFormat     = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.nX")
  val DisplayDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  val DisplayTimeFormat = DateTimeFormatter.ofPattern("HH:mm:ss")
  
  
  //  Extract the date converted to local time zone
  def extractISODate(isoDateTime: String): String = {
    val utc = ZonedDateTime.parse(isoDateTime, ISODateFormat)
    utc.withZoneSameInstant(ZoneId.systemDefault).toLocalDate.format(DisplayDateFormat)
  }

  //  Extract the time converted to local time zone
  def extractISOTime(isoDateTime: String): String = {
    val utc = ZonedDateTime.parse(isoDateTime, ISODateFormat)
    utc.withZoneSameInstant(ZoneId.systemDefault).toLocalTime.format(DisplayTimeFormat)
  }
}