
package svutil
import scala.util.Properties.{ propOrNone, envOrNone, isWin }



object Color {
  //  By default we use ANSI color escape codes unless we are not connected
  //  to a console, we are running on Windows or if the user has explicitly forbidden it.
  lazy val useColor = {
    val colorDefault = if (isWin) "no" else "yes"
    val ch = (envOrNone("SVNL1_COLOR") orElse propOrNone("svutil.color") getOrElse colorDefault).toLowerCase.head
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
      
  private def withColor(color: String, str: String): String =
    if (useColor)
      s"${color}${str}${RESET}"
    else
      str
    
  def red(str: String)    = withColor(RED, str)
  def green(str: String)  = withColor(GREEN, str)
  def yellow(str: String) = withColor(YELLOW, str)
  def blue(str: String)   = withColor(BLUE, str)
  def purple(str: String) = withColor(PURPLE, str)
  def cyan(str: String)   = withColor(CYAN, str)
  def white(str: String)  = withColor(WHITE, str)
  def gray(str: String)   = withColor(GRAY, str)
  def reset(str: String)  = withColor(RESET, str)
}