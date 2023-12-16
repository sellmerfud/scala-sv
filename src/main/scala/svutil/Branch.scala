

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
    allBranches: Boolean              = false,
    branchesRegex:    Option[Regex]   = None,
    allTags:     Boolean              = false,
    tagsRegex:        Option[Regex]   = None,
    addBranchPrefixes: Vector[String] = Vector.empty,
    remBranchPrefixes: Vector[String] = Vector.empty,
    addTagPrefixes: Vector[String]    = Vector.empty,
    remTagPrefixes: Vector[String]    = Vector.empty,
    listPrefixes: Boolean             = false,
    path:        String               = ".") {
  
    def listBranches = allBranches || branchesRegex.nonEmpty
    def listTags     = allTags     || tagsRegex.nonEmpty
    def havePrefixOption =
      addBranchPrefixes.nonEmpty ||
      remBranchPrefixes.nonEmpty ||
      addTagPrefixes.nonEmpty    ||
      remTagPrefixes.nonEmpty    ||
      listPrefixes

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
        case (Some(regex), options) => options.copy(allBranches = false, branchesRegex = Some(regex))
        case (None, options)        => options.copy(allBranches = true,  branchesRegex = None)
      }
          
      optl[Regex]("-t", "--tags[=<regex>]", "Display list of tags in the repository") {
        case (Some(regex), options) => options.copy(allTags = false, tagsRegex = Some(regex))
        case (None, options)        => options.copy(allTags = true,  tagsRegex = None)
      }
          
      reqd[String]("", "--add-branch-prefix=<prefix>", "Add a branch prefix") {
        (prefix, options) =>
        if (!prefix.startsWith("^/"))
          throw new InvalidArgumentException("Branch prefix must start with ^/")
        val cleanPrefix = prefix.substring(2).chomp("/")
        options.copy(addBranchPrefixes = options.addBranchPrefixes :+ cleanPrefix)
      }

      reqd[String]("", "--rem-branch-prefix=<prefix>", "Remove a branch prefix") {
        (prefix, options) =>
        if (!prefix.startsWith("^/"))
          throw new InvalidArgumentException("Branch prefix must start with ^/")
        val cleanPrefix = prefix.substring(2).chomp("/")
        options.copy(remBranchPrefixes = options.remBranchPrefixes :+ cleanPrefix)
      }

      reqd[String]("", "--add-tag-prefix=<prefix>", "Add a tag prefix") {
        (prefix, options) =>
        if (!prefix.startsWith("^/"))
          throw new InvalidArgumentException("Tag prefix must start with ^/")
        val cleanPrefix = prefix.substring(2).chomp("/")
        options.copy(addTagPrefixes = options.addTagPrefixes :+ cleanPrefix)
      }

      reqd[String]("", "--rem-tag-prefix=<prefix>", "Remove a tag prefix") {
        (prefix, options) =>
        if (!prefix.startsWith("^/"))
          throw new InvalidArgumentException("Tag prefix must start with ^/")
        val cleanPrefix = prefix.substring(2).chomp("/")
        options.copy(remTagPrefixes = options.remTagPrefixes :+ cleanPrefix)
      }

      flag("", "--list-prefixes", "List all branch and tag prefixes") {
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
    }
    
    parser.parse(args, Options())
  }
  
  private def prefixOperations(options: Options): Unit = {

    if (options.addBranchPrefixes.nonEmpty || options.remBranchPrefixes.nonEmpty) {
      var prefixes = svn.loadBranchPrefixes()

      prefixes = prefixes :++ options.addBranchPrefixes
      prefixes = prefixes.filterNot { prefix =>
        options.remBranchPrefixes.contains(prefix)
      }
      svn.saveBranchPrefixes(prefixes)
    }

    if (options.addTagPrefixes.nonEmpty || options.remTagPrefixes.nonEmpty) {
      var prefixes = svn.loadTagPrefixes()

      prefixes = prefixes :++ options.addTagPrefixes
      prefixes = prefixes.filterNot { prefix =>
        options.remTagPrefixes.contains(prefix)
      }
      svn.saveTagPrefixes(prefixes)
    }
    
    if (options.listPrefixes) {
      println("Branch prefixes")
      println("-----------------------------------------")
      for (prefix <- svn.getBranchPrefixes().sorted)
        println(s"^/$prefix")

      println("\nTag prefixes")
      println("-----------------------------------------")
      for (prefix <- svn.getTagPrefixes().sorted)
        println(s"^/$prefix")
    }
  }

  private def showList(options: Options): Unit = {
    
    def listEntries(header: String, baseUrl: String, prefixes: List[String], regex: Option[Regex]): Unit = {
      val allPrefixes = svn.getBranchPrefixes() ::: svn.getTagPrefixes()
      //  We skip paths that match branch prefixes or tag prefixes.
      //  If the regex is empty then we are matching all entries
      def acceptable(path: String): Boolean = {
        !allPrefixes.contains(path)  &&   // 
        (regex map (_.contains(path)) getOrElse true)
      }
      println()
      println(header)
      println("--------------------")
      
      for {
        prefix <- prefixes
        entry  <- svn.pathList(joinPaths(baseUrl, prefix)).head.entries
        path = joinPaths(prefix, entry.name)
        if acceptable(path)
      } {
        println(green(joinPaths("^", path)))
      }
    }
    
    // Returns list of strings sorted by length. Longest first.
    def sortedByLength(list: List[String]): List[String] = list.sortBy(p => -p.length)

    def baseUrl: String = {
      val branches = sortedByLength(svn.getBranchPrefixes()).mkString("|")
      val tags     = sortedByLength(svn.getTagPrefixes()).mkString("|")
      
      val baseMatch = s"""(.*?)/(?:trunk|$branches|$tags)(?:/.*)?""".r
      svn.info(options.path).url match {
        case baseMatch(base) => base
        case _               => generalError(s"Cannot determine the base URL for ${options.path}")
      }
    }
    
    if (options.listBranches)
      listEntries("Branches", baseUrl, svn.getBranchPrefixes().sorted, options.branchesRegex)
    
    if (options.listTags)
      listEntries("Tags", baseUrl, svn.getTagPrefixes().sorted, options.tagsRegex)
  }
  
  private def showCurrentBranch(options: Options): Unit = {
    if (options.path.startsWith("^") || options.path.contains("://"))
      generalError("Cannot show the current branch of a URL")
    val p = new java.io.File(options.path)
    val path = if (p.isAbsolute) os.Path(p) else os.pwd / os.RelPath(p)
    val (branch, revision) = svn.currentBranch(path)
    println(s"Current branch: ${green(branch)} [${yellow(revision)}]")
  }
  
  override def run(args: Seq[String]): Unit = {
    val options = processCommandLine(args)
    //  If one or more of the prefix options has been given
    //  then we only operate on prefixes
    if (options.havePrefixOption)
      prefixOperations(options)
    else if (options.listBranches || options.listTags)
      showList(options)
    else
      showCurrentBranch(options)
  } 
}
