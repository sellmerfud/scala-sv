

package svutil

import scala.util.matching.Regex
import java.util.regex.PatternSyntaxException
import scala.xml._
import Color._
import Utilities._
import svn.model.{ ListEntry }

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
      separator("")
      separator(description)
      separator("Options:")
      
      optl[Regex]("-b", "--branches[=<regex>]", "Display list of branches in the repository") {
        case (Some(regex), options) => options.copy(allBranches = false, branches = Some(regex))
        case (None, options)        => options.copy(allBranches = true,  branches = None)
      }
          
      optl[Regex]("-t", "--tags[=<regex>]", "Display list of tags in the repository") {
        case (Some(regex), options) => options.copy(allTags = false, tags = Some(regex))
        case (None, options)        => options.copy(allTags = true,  tags = None)
      }
          
      flag("-h", "--help", "Show this message")
          { _ => println(help); throw HelpException() }
    
      arg[String] { (path, options) => options.copy(path = path) }  
        
      separator("")
      separator("If neither of --branches or --tags is present, the current branch is displayed.")
      separator("If no <regex> is specified for --branches, --tags then all are listed.")
      separator("If <path> is omitted the current directory is used by default.")
      separator("Use -- to separate the <path> from --branches or --tag with no <regex>")
      separator("Assumes the repository has standard /trunk, /branches, /tags structure.")
    }
    
    parser.parse(args, Options())
  }
  
  
  private def showList(options: Options): Unit = {
    
    def listEntries(header: String, url: String, regex: Option[Regex]): Unit = {
      //  If the regex is empty the we are matching all entries
      def acceptable(entry: ListEntry): Boolean = regex map (_.contains(entry.name)) getOrElse true
      println()
      println(header)
      println("--------------------")
      
      val x = svn.pathList(url)
      svn.pathList(url).head.entries filter acceptable foreach { entry =>
        println(green(entry.name))
      }
    }
    
    val baseMatch = """(.*?)/(?:trunk|branches|tags)(?:/.*)?""".r
    val baseUrl = svn.info(options.path).url match {
        case baseMatch(base) => base
        case _               => generalError(s"Cannot find the '$name' directory for the repository")
      }
    
        
    if (options.listBranches)
      listEntries("Branches", s"$baseUrl/branches", options.branches)
    
    if (options.listTags)
      listEntries("Tags", s"$baseUrl/tags", options.tags)
  }
  
  private def showCurrentBranch(options: Options): Unit = {
    val (branch, revision) = svn.currentBranch(options.path)
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
