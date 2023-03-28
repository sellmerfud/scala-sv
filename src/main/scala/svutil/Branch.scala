

package svutil

import scala.util.matching.Regex
import scala.util.Properties.propOrElse
import scala.xml._
import Exec.runCmd
import Color._
import svutil.exceptions._
import Utilities._

object Branch extends Command {
  
  override val name = "br"
  override val description = "Display current branch or list branches"
  
  case class Options(
    branches: Boolean        = false,
    tags:     Boolean        = false,
    prefix:   Option[String] = None,
    path:     String         = ".")
  
  private def processCommandLine(args: Seq[String]): Options = {
    import org.sellmerfud.optparse._
    
    val scriptName = propOrElse("svutil.script.name", "sv")
    
    val parser = new OptionParser[Options] {
      banner = s"usage: $scriptName $name [<options>] [<path> | <url>]"
      
      flag("-b", "--branches", "Dispaly list of branches in the repo")
          { _.copy(branches = true) }
          
      flag("-t", "--tags", "Dispaly list of tags in the repo")
          { _.copy(tags = true) }
          
      reqd[String]("-p", "--prefix=<string>", "Only list entries that begin with <string>")
        { (string, options) => options.copy(prefix = Some(string)) }

      flag("-h", "--help", "Show this message")
          { _ => println(help); throw HelpException() }
    
      arg[String] { (path, options) => options.copy(path = path) }  
        
      separator("")
      separator("If neither of --branches or --tags is present, the current branch is displayed.")
      separator("If <path> is omitted the current directory is used by default.")
      separator("Assumes the repo has standard /trunk, /branches, /tags structure.")
    }
    
   parser.parse(args, Options())
  }
  
  
  private def showList(options: Options): Unit = {
    
    def matchesPrefix(name: String) = options.prefix map (p => name.startsWith(p)) getOrElse true
    def showEntries(segments: List[(String, SvnList)]): Unit = segments match {
      case Nil =>
      case (header, svnList)::rest =>
        println()
        println(green(header))
        println(green("--------------------"))
        for (ListEntry(name, kind, size, commitRev, commitAuthor, commitDate) <- svnList.entries if matchesPrefix(name))
          println(name)
        showEntries(rest)
    }
    
    val info = getSvnInfo(options.path)
    val (branchesHeader, branchesUrl) = if (options.branches)
       (Some("Branches"), Seq(s"${info.rootUrl}/branches"))
    else
      (None, Seq.empty)
    val (tagsHeader, tagsUrl) = if (options.tags)
      (Some("Tags"), Seq(s"${info.rootUrl}/tags"))
    else
      (None, Seq.empty)
    
    val lists = getSvnLists((branchesUrl ++ tagsUrl): _*)
    val headers = branchesHeader.toList ::: tagsHeader.toList
    
    showEntries(headers zip lists)
  }
  
  private def showCurrentBranch(options: Options): Unit = {

    val info = getSvnInfo(options.path)
    val TRUNK  = """\^/trunk.*""".r
    val BRANCH = """\^/branches/([^/]+).*""".r
    val TAG    = """\^/tags/([^/]+).*""".r
    // Parse the XML log entries
    val branch = info.relativeUrl match {
      case TRUNK()      => "trunk"
      case BRANCH(name) => name
      case TAG(name)    => s"tag:$name"
      case _            => "cannot be determined"
    }
    println(s"Current branch: ${yellow(branch)}")
  }
  
  override def run(args: Seq[String]): Unit = {
    val options = processCommandLine(args)
    
    if (options.branches || options.tags)
      showList(options)
    else
      showCurrentBranch(options)
  } 
}
