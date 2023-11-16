

package svutil

import scala.util.matching.Regex
import java.util.regex.PatternSyntaxException
import java.time._
import scala.xml._
import scala.util.{ Try, Success, Failure }
import Color._
import Utilities._
import svn.model.SvnInfo
import svn.model.{ ListEntry }

object FileRevs extends Command {
  
  override val name = "filerevs"
  override val description = "Display commit revisions of files across tags and branches"
  
  case class Options(
    allBranches: Boolean        = false,
    allTags:     Boolean        = false,
    branches:    Vector[Regex]  = Vector.empty,
    tags:        Vector[Regex]  = Vector.empty,
    paths:       Vector[String] = Vector.empty)
  
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
      
      banner = s"usage: $scriptName $name [<options>] <path> | <url>..."
      separator("")
      separator(description)
      separator("Options:")
      
      optl[Regex]("-b", "--branch[=<regex>]", "Specifiy branch(es) upon which to reference paths") {
        case (Some(regex), options) => options.copy(branches = options.branches :+ regex)
        case (None, options )       => options.copy(allBranches = true)
      }
          
      optl[Regex]("-t", "--tag[=<regex>]", "Specifiy tag(s) upon which to reference paths") {
        case (Some(regex), options) => options.copy(tags = options.tags :+ regex)
        case (None, options)        => options.copy(allTags = true)
      }
        
      flag("-h", "--help", "Show this message")
          { _ => println(help); throw HelpException() }
    
      arg[String] { (path, options) => options.copy(paths = options.paths :+ path) }  
        
      separator("")
      separator("If neither --branch nor --tag is given, only the trunk revision is displayed.")
      separator("--branch and --tag may be specified multiple times.")
      separator("--branch or --tag without a <regex> will consider all branches or tags")
      separator("Use -- to separate the <path> from --branches or --tag with no <regex>")
      separator("Assumes the repository has standard /trunk, /branches, /tags structure.")
    }
    
    parser.parse(args, Options())
  }
  
  
  private def getBranches(rootUrl: String, options: Options): List[String] = {
    if (options.allBranches == false && options.branches.isEmpty)
      Nil
    else {
      val prefixes = svn.getBranchPrefixes().sorted
      def acceptable(branch: String): Boolean = {
        !prefixes.contains(branch)  &&
        (options.allBranches || (options.branches.exists(_.contains(branch))))
      }
      for {
        prefix <- prefixes
        entry  <- svn.pathList(joinPaths(rootUrl, prefix)).head.entries
        branch = joinPaths(prefix, entry.name)
        if acceptable(branch)
      } yield branch
    }
  }
  
  private def getTags(rootUrl: String, options: Options): List[String] = {
    if (options.allTags == false && options.tags.isEmpty)
      Nil
    else {
      val tagEntries = svn.pathList(joinPaths(rootUrl, "tags")).head.entries
      if (options.allTags)
        tagEntries map (e => s"tags/${e.name}")
      else
        tagEntries filter { entry => options.tags.exists(_.contains(entry.name)) } map (e => s"tags/${e.name}")
    }
  }
  
  private def getTrunk(rootUrl: String): String = {
    if (svn.pathList(rootUrl).head.entries exists (_.name == "trunk"))
      "trunk"
    else
      generalError(s"Cannot find repository trunk: ${joinPaths(rootUrl, "trunk")}")
  }
  
  //  We must determine the path to the file relative
  //  to its subversion prefix, where the prefix is one of:
  //    ^/trunk
  //    ^/tags/<tag-name>
  //    ^/<branch-prefix>/<branch-name>
  //  
  private def getSvnRelativePath(relativeUrl: String, locations: List[String]): String = {
    val sortedLocs = locations.sortBy(s => -s.length)

    sortedLocs find (loc => relativeUrl.startsWith(s"^/$loc")) match {
      case Some(loc) => relativeUrl.substring(loc.length + 3)
      case None => generalError(s"Cannot determine relative path for $relativeUrl")
    }
  }
  
  private def maxWidth(seq: Seq[String]) = seq.foldLeft(0) ((maxLen, str) => str.length max maxLen)
  
  
  private def formatField(value: String, width: Int, rightJustify: Boolean = false): String = {
    val pad = " " * (width - value.length)
    if (rightJustify) s"$pad$value" else s"$value$pad"
  }
  
  
  // /this/is/the/users/path              
  // Location        Repo Rev  Commit Rev  Author  Date         Size
  // --------------  --------  ----------  ------  -----------  ----------
  // trunk               7601   <does not exist>
  // branches/8.1        7645
  // tags/8.1.1-GA       7625
  // 7630                7630
  
  private def showResults(rootUrl: String, pathEntry: SvnInfo, locations: List[String]): Unit = {
    case class Result(locationName: String, info: Option[SvnInfo])
    val relPath = getSvnRelativePath(pathEntry.relativeUrl, locations)
    println(s"DEBUG: relativeUrl=${pathEntry.relativeUrl}, relPath=$relPath")
    val results = for (location <- locations) yield {
      val entryPath = joinPaths(rootUrl, location, relPath)
      
      Try(svn.info(entryPath, Some("HEAD"))) match {
        case Success(info) => Result(s"^/$location", Some(info))
        case Failure(e)    => Result(s"^/$location", None)
      }
    }
    
    val Location        = "Location"
    val Revision        = "Revision"
    val Author          = "Author"
    val Date            = "Date"
    val Size            = "Size"
    val LocationWidth   = maxWidth(Location :: (results map (_.locationName)))
    val RevisionWidth   = maxWidth(Revision :: (results filter (_.info.nonEmpty) map (_.info.get.commitRev)))
    val AuthorWidth     = maxWidth(Author :: (results filter (_.info.nonEmpty) map (_.info.get.commitAuthor)))
    val DateWidth       = displayDateTime(LocalDateTime.now).length
    val SizeWidth       = 8
    val ColumnSeparator = " "
    
    // Display results
    println()
    if (pathEntry.kind == "dir")
      println(blue(s"$relPath/"))
    else
      println(blue(relPath))
    // Headers
    print(formatField(Location, LocationWidth)); print(ColumnSeparator)
    print(formatField(Revision, RevisionWidth)); print(ColumnSeparator)
    print(formatField(Author, AuthorWidth)); print(ColumnSeparator)
    print(formatField(Date, DateWidth)); print(ColumnSeparator)
    print(formatField(Size, SizeWidth)); println()
    
    print("-" * LocationWidth); print(ColumnSeparator)
    print("-" *  RevisionWidth); print(ColumnSeparator)
    print("-" *  AuthorWidth); print(ColumnSeparator)
    print("-" *  DateWidth); print(ColumnSeparator)
    print("-" *  SizeWidth); println()

    results foreach {
      case Result(locName, None) =>
        print(green(formatField(locName, LocationWidth))); print(ColumnSeparator)
        println(red("<does not exist>"))
        
      case Result(locName, Some(info)) =>
        val date = displayDateTime(info.commitDate)
        val size = info.size map (_.toString) getOrElse "n/a"
        print(green(formatField(locName, LocationWidth))); print(ColumnSeparator)
        print(yellow(formatField(info.commitRev, RevisionWidth, true))); print(ColumnSeparator)
        print(cyan(formatField(info.commitAuthor, AuthorWidth))); print(ColumnSeparator)
        print(purple(formatField(date, DateWidth))); print(ColumnSeparator)
        print(formatField(size, SizeWidth, true)); println()
    }
  }
    
  override def run(args: Seq[String]): Unit = {
    val options = processCommandLine(args)
    
    if (options.paths.isEmpty)
      generalError("At least one path must be specified.")
      
    val pathList = svn.infoList(options.paths filterNot (_.trim == ""))
    
    // First make sure all paths are rooted in the same repository
    if ((pathList map (_.repoUUID)).distinct.size != 1)
      generalError("All paths must refer to the same repository.")
    
    val rootUrl  = pathList.head.rootUrl
    val locations = getTrunk(rootUrl) :: getBranches(rootUrl, options) ::: getTags(rootUrl, options)
    
    for (pathEntry <- pathList)
      showResults(rootUrl, pathEntry, locations)
  } 
}
