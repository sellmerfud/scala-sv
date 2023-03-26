
package svutil

trait Command {
  val name: String
  val description: String
  def run(args: Seq[String]): Unit
}

