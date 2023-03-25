

package svutil

import scala.util.matching.Regex
import scala.util.Properties.propOrElse
import scala.xml._
import Exec.exec
import Color._

object Branch extends Command {
  
  override val name = "br"
  override val description = "Display current branch or list branches"
  
  case class Options(
    branches: Boolean        = false,
    tags:     Boolean        = false,
    prefix:   Option[String] = None,
    path:     String         = ".")
  
  private def processCommandLine(args: Seq[String]): Option[Options] = {
    import org.sellmerfud.optparse._
    
    val scriptName = propOrElse("svutil.script.name", "sv")
    
    try {
      val parser = new OptionParser[Options] {
        banner = s"usage: $scriptName $name [<options>] [<path> | <url>]"
        
        flag("-b", "--branches", "Dispaly list of branches in the repo")
            { _.copy(branches = true) }
            
        flag("-t", "--tags", "Dispaly list of tags in the repo")
            { _.copy(tags = true) }
            
        reqd[String]("-p", "--prefix=<string>", "Only list entries that begin with <string>")
          { (string, options) => options.copy(prefix = Some(string)) }

        flag("-h", "--help", "Show this message")
            { _ => println(help); sys.exit(0) }
      
        arg[String] { (path, options) => options.copy(path = path) }  
          
        separator("")
        separator("If neither of --branches or --tags is present, the current branch is displayed.")
        separator("If <path> is omitted the current directory is used by default.")
        separator("Assumes the repo has standard /trunk, /branches, /tags structure.")
      }
      
      Some(parser.parse(args, Options()))
    }
    catch {
      case e: OptionParserException =>
        println(e.getMessage)
        None
    }
  }
  
  
  private def showList(options: Options): Int = {
    
    def matchesPrefix(name: String) = options.prefix map (p => name.startsWith(p)) getOrElse true
    def showEntries(headers: List[String], lists: NodeSeq): Unit = {
      headers match {
        case Nil =>
        
        case h::hs =>
          println()
          println(green(h))
          println(green("--------------------"))
          for {
            entry <- (lists.head \ "entry")
            name = (entry \ "name").head.text
            if matchesPrefix(name)
          } {
            println(name)
          }
            
          showEntries(hs, lists drop 1)
      }
    }
    
    val (infoStatus, infoOut, infoErr) = exec(Seq("svn", "info", "--xml", options.path))
    
    if (infoStatus == 0) {
      val rootUrl: String = (XML.loadString(infoOut.mkString("\n")) \ "entry" \ "repository" \ "root").head.text
      val (branchesHeader, branchesUrl) = if (options.branches)
         (Some("Branches"), Seq(s"$rootUrl/branches"))
      else
        (None, Seq.empty)
      val (tagsHeader, tagsUrl) = if (options.tags)
        (Some("Tags"), Seq(s"$rootUrl/tags"))
      else
        (None, Seq.empty)
      val cmdLine = Seq("svn", "ls", "--xml") :++ branchesUrl :++ tagsUrl
      
      val (status, out, err) = exec(cmdLine)
      if (status == 0) {
        val headers = branchesHeader.toList ::: tagsHeader.toList
        val lists   = (XML.loadString(out.mkString("\n")) \ "list")
        showEntries(headers, lists)
        0
      }
      else {
        err foreach println
        status
      }
    }
    else {
      infoErr foreach println
      infoStatus
    }
  }
  
  private def showCurrentBranch(options: Options): Int = {

    val (status, out, err) = exec(Seq("svn", "info", "--xml", options.path))
    if (status == 0) {
      val TRUNK  = """\^/trunk.*""".r
      val BRANCH = """\^/branches/([^/]+).*""".r
      val TAG    = """\^/tags/([^/]+).*""".r
      // Parse the XML log entries
      val relativeURL = (XML.loadString(out.mkString("\n")) \ "entry" \ "relative-url").head.text
      val branch = relativeURL match {
        case TRUNK()      => "trunk"
        case BRANCH(name) => name
        case TAG(name)    => s"tag:$name"
        case _            => "cannot be determined"
      }
      println(s"Current branch: ${yellow(branch)}")
    }
    else
      err foreach println
    status
  }
  
  override def run(args: Seq[String]): Int = {
    processCommandLine(args) match {
      case Some(options) if (options.branches || options.tags) => showList(options)
      case Some(options)                                       => showCurrentBranch(options)
      case None                                                => 1  // Command line failed
    }    
  } 
}
