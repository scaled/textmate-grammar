//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import java.io.{File, InputStream, PrintStream}
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, Map => MMap}
import scaled._

/** Contains the data for a TextMate language grammar. Certain elements are omitted as they do not
  * map directly to the way Scaled handles languages. Generally one would create a language mode in
  * Scaled, and then make use of the TextMate grammar to handle syntax highlighting and other
  * mechanisms.
  */
abstract class Grammar (
  val name               :String,
  val scopeName          :String,
  val foldingStartMarker :Option[String],
  val foldingStopMarker  :Option[String]
) {
  val repository :Map[String,Rule]
  val patterns   :List[Rule]

  override def toString =
    s"Grammar[$name, $scopeName, fStart=$foldingStartMarker, fStop=$foldingStopMarker]"

  /** Prints a debug representation of this grammar to `out`. */
  def print (out :PrintStream) {
    out.println(this)
    repository foreach { case (k, v) => out.println(k) ; v.print(out, 1) }
    patterns foreach { _.print(out, 0) }
  }

  /** A rule which includes a group of rules from the repository. */
  protected def include (group :String) :Rule = new Rule.Include(group)

  /** A rule that just contains more rules. This usually only shows up in the repository. */
  protected def rules (rules :Rule*) :Rule = new Rule.Container(rules.toList)

  /** A rule which matches a regular expression on a single line, optionally assigning a name to the
    * entire regexp match, and to invidual capture groups identified by the regexp. */
  protected def single (pattern :String, name :Option[String] = None,
                        captures :List[(Int,String)] = Nil) :Rule = new Rule.Single(
    pattern, name, captures)

  /** A rule which matches a `begin` regular expression and an `end` regular expression (which may be
    * one separate lines), and which activates a new collection of rules when matching the code in
    * between `begin` and `end`. */
  protected def multi (begin :String, beginCaptures :List[(Int,String)] = Nil,
                       end :String, endCaptures :List[(Int,String)] = Nil,
                       name :Option[String] = None, contentName :Option[String] = None,
                       patterns :List[Rule] = Nil) :Rule = new Rule.Multi(
    begin, beginCaptures, end, endCaptures, name, contentName, patterns)

  /** A multiline rule which applies the same `captures` to the `begin` and `end` patterns. */
  protected def multi (begin :String, end :String, captures :List[(Int,String)],
                       name :Option[String], contentName :Option[String],
                       patterns :List[Rule]) :Rule = multi(
    begin, captures, end, captures, name, contentName, patterns)

  /** Useful for generating multiple rules that are variations on a theme. */
  protected def map (names :String*)(fn :String => Rule) = names map fn
}

object Grammar {
  import scala.collection.convert.WrapAsScala._
  import com.dd.plist._

  /** Compiles `grammars` which are a set of inter-related grammars, and returns the matchers from
    * the last grammar in the list. The last grammar is presumed to be the main grammar for the
    * mode and the other grammars are for languages which can be nested in the main grammar.
    */
  def compile (grammars :Seq[Grammar]) :List[Matcher] = {
    val compilers = MMap[String,Compiler]()
    grammars foreach { g => compilers += (g.scopeName -> new Compiler(compilers, g)) }
    compilers(grammars.last.scopeName).matchers
  }

  /** Parses a `tmLanguage` grammar file which should be in plist XML format. */
  def parse (file :File) :Grammar = toGrammar(PropertyListParser.parse(file))

  /** Parses a `tmLanguage` grammar description, which should be in plist XML format. */
  def parse (in :InputStream) :Grammar = toGrammar(PropertyListParser.parse(in))

  private def toGrammar (root :NSObject) :Grammar = {
    val rootDict = root.asInstanceOf[NSDictionary]
    val name = stringFor(rootDict, "name") getOrElse "unknown:name"
    val scopeName = stringFor(rootDict, "scopeName") getOrElse "unknown:scopeName"
    val foldStart = stringFor(rootDict, "foldingStartMarker")
    val foldStop  = stringFor(rootDict, "foldingStopMarker")

    // val fileTypes = rootDict.objectForKey("fileTypes")
    new Grammar(name, scopeName, foldStart, foldStop) {
      val repository = Map() ++ dictFor(rootDict, "repository").getHashMap map {
        case (k, v) => (k -> parseRule(v))
      }
      val patterns = parseRules(rootDict)
    }
  }

  private def stringFor (dict :NSDictionary, key :String) = dict.objectForKey(key) match {
    case nsstr :NSString => Some(nsstr.toString)
    case _ => None
  }
  private def dictFor (dict :NSDictionary, key :String) = dict.objectForKey(key) match {
    case dval :NSDictionary => dval
    case _ => new NSDictionary()
  }
  private def arrayFor (dict :NSDictionary, key :String) = dict.objectForKey(key) match {
    case aval :NSArray => aval
    case _ => new NSArray()
  }

  private def parseCaptures (dict :NSDictionary) = List() ++ dict flatMap {
    case (group, vdict :NSDictionary) => Some(group.toInt -> stringFor(vdict, "name").get)
    case _ => None
  }

  private def parseRules (dict :NSDictionary) :List[Rule] =
    arrayFor(dict, "patterns").getArray.map(parseRule).toList

  private def parseRule (data :NSObject) :Rule = {
    val dict = data.asInstanceOf[NSDictionary]
    val include = stringFor(dict, "include")
    val name = stringFor(dict, "name")
    val `match` = stringFor(dict, "match")
    val begin = stringFor(dict, "begin")

    if (include.isDefined) {
      new Rule.Include(include.get)
    } else if (begin.isDefined) {
      val caps = parseCaptures(dictFor(dict, "captures"))
      val beginCaps = if (!dict.containsValue("beginCaptures")) caps else
        parseCaptures(dictFor(dict, "beginCaptures"))
      val end = stringFor(dict, "end")
      val endCaps = if (!dict.containsValue("endCaptures")) caps else
        parseCaptures(dictFor(dict, "endCaptures"))
      new Rule.Multi(begin.get, beginCaps, end.get, endCaps, name, stringFor(dict, "contentName"),
                     parseRules(dict))
    } else if (`match`.isDefined) {
      new Rule.Single(`match`.get, name, parseCaptures(dictFor(dict, "captures")))
    } else {
      new Rule.Container(parseRules(dict))
    }
  }

  private class Compiler (compilers :MMap[String,Compiler], grammar :Grammar) {
    val cache = MMap[String, List[Matcher]]()
    val incFn = (_ :String) match {
      case "$self" => matchers
      case group if (group startsWith "#") => cache.get(group substring 1) match {
        case Some(ms) => ms
        case None     => println(s"Unknown include [grammar=${grammar.name}, group=$group]") ; Nil
      }
      case lang => compilers.get(lang) match {
        case Some(comp) => comp.matchers
        case None       => println(s"Unknown include [grammar=${grammar.name}, lang=$lang]") ; Nil
      }
    }
    grammar.repository foreach {
      case (k, v) => cache put (k, v.compile(incFn))
    }
    lazy val matchers :List[Matcher] = grammar.patterns.flatMap(_.compile(incFn))
  }
}
