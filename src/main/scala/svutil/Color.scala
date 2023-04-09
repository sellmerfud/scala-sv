
package svutil
import scala.util.Properties.{ propOrNone, envOrNone, isWin }



object Color {
  //  By default we use ANSI color escape codes unless we are not connected
  //  to a console, we are running on Windows or if the user has explicitly forbidden it.
  lazy val useColor = {
    val colorDefault = if (isWin) "no" else "yes"
    val ch = (envOrNone("SV_COLOR") orElse propOrNone("sv.color") getOrElse colorDefault).toLowerCase.head
    System.console != null && "yt1".toSet(ch)
  }
      
  val RED    = "\u001b[31m"
  val GREEN  = "\u001b[32m"
  val YELLOW = "\u001b[33m"
  val BLUE   = "\u001b[34m"
  val PURPLE = "\u001b[35m"
  val CYAN   = "\u001b[36m"
  val WHITE  = "\u001b[37m"
  val GRAY   = "\u001b[90m"
  val RESET  = "\u001b[0m" 
      
  private def withColor(color: String)(str: String): String =
    if (useColor)
      s"${color}${str}${RESET}"
    else
      str
    
  val red    = withColor(RED) _
  val green  = withColor(GREEN) _
  val yellow = withColor(YELLOW) _
  val blue   = withColor(BLUE) _
  val purple = withColor(PURPLE) _
  val cyan   = withColor(CYAN) _
  val white  = withColor(WHITE) _
  val gray   = withColor(GRAY) _
}