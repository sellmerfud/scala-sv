

package svutil

import scala.util.matching.Regex
import java.util.regex.PatternSyntaxException
import scala.xml._
import Exec.runCmd
import Color._
import Utilities._

object Branch extends Command {
  
  override val name = "branch"
  override val description = "Display current branch or list branches"
  
  case class Options(
    allBranches: Boolean       = false,
    branches:    Option[Regex] = None,
    allTags:     Boolean       = false,
    tags:        Option[Regex] = None,
    path:        String        = ".") {
  
    def listBranches = allBranches || branches.nonEmpty
    def listTags     = allTags     || tags.nonEmpty
  }
    
  
  private def processCommandLine(args: Seq[String]): Options = {
    import org.sellmerfud.optparse._
    
    val parser = new OptionParser[Options] {
      addArgumentParser[Regex] { arg =>
        try   { new Regex(arg) }
        catch { 
          case e: PatternSyntaxException =>
            throw new InvalidArgumentException(s"\n${e.getMessage}")
        }
      }

      banner = s"usage: $scriptName $name [<options>] [<path> | <url>]"
      
      optl[Regex]("-b", "--branches[=<regex>]", "Display list of branches in the repository") {
        (regex, options) => 
            if (regex.isEmpty)
              options.copy(allBranches = true, branches = None)
            else
              options.copy(allBranches = false, branches = regex)
      }
          
      optl[Regex]("-t", "--tags[=<regex>]", "Display list of tags in the repository") {
        (regex, options) => 
          if (regex.isEmpty)
            options.copy(allTags = true, tags = None)
          else
            options.copy(allTags = false, tags = regex)
      }
          
      flag("-h", "--help", "Show this message")
          { _ => println(help); throw HelpException() }
    
      arg[String] { (path, options) => options.copy(path = path) }  
        
      separator("")
      separator("If neither of --branches or --tags is present, the current branch is displayed.")
      separator("If no <regex> is specified for --branches, --tags then all are listed.")
      separator("If <path> is omitted the current directory is used by default.")
      separator("Assumes the repository has standard /trunk, /branches, /tags structure.")
    }
    
    parser.parse(args, Options())
  }
  
  
  private def showList(options: Options): Unit = {
    
    def listEntries(header: String, url: String, regex: Option[Regex]): Unit = {
      //  If the regex is empty the we are matching all entries
      def acceptable(entry: ListEntry): Boolean = regex map (_.findFirstIn(entry.name).nonEmpty) getOrElse true
      println()
      println(header)
      println("--------------------")
      
      val x = getSvnLists(url)
      getSvnLists(url).head.entries filter acceptable foreach { entry =>
        println(green(entry.name))
      }
    }
    
    val baseMatch = """(.*?)/(?:trunk|branches|tags)(?:/.*)?""".r
    val baseUrl = getSvnInfo(options.path).url match {
        case baseMatch(base) => base
        case _               => generalError(s"Cannot find the '$name' directory for the repository")
      }
    
        
    if (options.listBranches)
      listEntries("Branches", s"$baseUrl/branches", options.branches)
    
    if (options.listTags)
      listEntries("Tags", s"$baseUrl/tags", options.tags)
  }
  
  private def showCurrentBranch(options: Options): Unit = {
    val (branch, revision) = getCurrentBranch(options.path)
    println(s"Current branch: ${green(branch)} [${yellow(revision)}]")
  }
  
  override def run(args: Seq[String]): Unit = {
    val options = processCommandLine(args)
    
    if (options.listBranches || options.listTags)
      showList(options)
    else
      showCurrentBranch(options)
  } 
}
