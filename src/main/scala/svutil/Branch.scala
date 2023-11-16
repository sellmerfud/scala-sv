

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
    allBranches: Boolean        = false,
    branches:    Option[Regex]  = None,
    allTags:     Boolean        = false,
    tags:        Option[Regex]  = None,
    addPrefixes: Vector[String] = Vector.empty,
    remPrefixes: Vector[String] = Vector.empty,
    listPrefixes: Boolean       = false,
    path:        String         = ".") {
  
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
          
      reqd[String]("-a", "--add-prefix=<prefix>", "Add a branch prefix") {
        (prefix, options) =>
        if (!prefix.startsWith("^/"))
          throw new InvalidArgumentException("Branch prefix must start with ^/")
        val cleanPrefix = prefix.substring(2).chomp("/")
        options.copy(addPrefixes = options.addPrefixes :+ cleanPrefix)
      }

      reqd[String]("-d", "--delete-prefix=<prefix>", "Delete a branch prefix") {
        (prefix, options) =>
        if (!prefix.startsWith("^/"))
          throw new InvalidArgumentException("Branch prefix must start with ^/")
        val cleanPrefix = prefix.substring(2).chomp("/")
        options.copy(remPrefixes = options.remPrefixes :+ cleanPrefix)
      }

      flag("-l", "--list-prefix", "List all branch prefixes") {
        (options) =>
        options.copy(listPrefixes = true)
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
  
  private def prefixOperations(options: Options): Unit = {

    if (options.addPrefixes.nonEmpty || options.remPrefixes.nonEmpty) {
      var prefixes = svn.loadBranchPrefixes()

      prefixes = prefixes :++ options.addPrefixes
      prefixes = prefixes.filterNot { prefix =>
        options.remPrefixes.contains(prefix)
      }
      svn.saveBranchPrefixes(prefixes)
    }
    
    if (options.listPrefixes)
      for (prefix <- svn.getBranchPrefixes())
        println(s"^/$prefix")
  }

  private def showList(options: Options): Unit = {
    
    def listEntries(header: String, baseUrl: String, prefixes: List[String], regex: Option[Regex]): Unit = {
      //  We skip entries that match branch prefixes.
      //  If the regex is empty the we are matching all entries
      def acceptable(branch: String): Boolean = {
        !prefixes.contains(branch)  &&   // 
        (regex map (_.contains(branch)) getOrElse true)
      }
      println()
      println(header)
      println("--------------------")
      
      for {
        prefix <- prefixes
        entry  <- svn.pathList(joinPaths(baseUrl, prefix)).head.entries
        branch = joinPaths(prefix, entry.name)
        if acceptable(branch)
      } {
        println(green(joinPaths("^", branch)))
      }
    }
    
    def baseUrl: String = {
      val branches = svn.getBranchPrefixesSorted().mkString("|")
      
      val baseMatch = s"""(.*?)/(?:trunk|$branches|tags)(?:/.*)?""".r
      svn.info(options.path).url match {
        case baseMatch(base) => base
        case _               => generalError(s"Cannot find the '$name' directory for the repository")
      }
    }
    
        
    if (options.listBranches)
      listEntries("Branches", baseUrl, svn.getBranchPrefixes().sorted, options.branches)
    
    if (options.listTags)
      listEntries("Tags", baseUrl, List("tags"), options.tags)
  }
  
  private def showCurrentBranch(options: Options): Unit = {
    if (options.path.startsWith("^") || options.path.contains("://"))
      generalError("Cannot show the currernt branh of a URL")
    val p = new java.io.File(options.path)
    val path = if (p.isAbsolute) os.Path(p) else os.pwd / os.RelPath(p)
    val (branch, revision) = svn.currentBranch(path)
    println(s"Current branch: ${green(branch)} [${yellow(revision)}]")
  }
  
  override def run(args: Seq[String]): Unit = {
    val options = processCommandLine(args)
    //  If one or more of the prefix options has been given
    //  then we only operate on prefixes
    if (options.addPrefixes.nonEmpty || options.remPrefixes.nonEmpty || options.listPrefixes)
      prefixOperations(options)
    else if (options.listBranches || options.listTags)
      showList(options)
    else
      showCurrentBranch(options)
  } 
}
