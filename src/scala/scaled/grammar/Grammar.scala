//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import java.io.{File, InputStream, PrintStream}
import java.nio.file.Path
import java.util.HashMap
import scaled._
import scaled.util.Resource

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

  override def toString = s"Grammar[$name, $scopeName]"

  /** Prints a debug representation of this grammar to `out`. */
  def print (out :PrintStream) {
    val w = new NDF.Writer(out, 0)
    w.emit("name", name)
    w.emit("scopeName", scopeName)
    w.emit("foldStart", foldingStartMarker)
    w.emit("foldStop", foldingStopMarker)
    val rw = w.nest("repository")
    repository.toSeq.sortBy(_._1) foreach { case (k, v) => v.print(rw.nest(k)) }
    val pw = w.nest("patterns")
    patterns foreach { _.print(pw) }
  }

  /** Returns all scope names used by all rules in this grammar. */
  def scopeNames () :Set[String] = {
    val names = Set.builder[String]
    repository.values foreach { _.collectNames(names) }
    patterns foreach { _.collectNames(names) }
    names.buildSorted()
  }

  /** A rule which includes a group of rules from the repository. */
  protected def include (group :String) :Rule = new Rule.Include(group)

  /** A rule that just contains more rules. This usually only shows up in the repository. */
  protected def rules (rules :List[Rule]) :Rule = new Rule.Container(rules)

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
  protected def map (names :String*)(fn :String => Rule) :List[Rule] = List.copyOf(names) map fn
}

object Grammar {
  import com.dd.plist._

  /** Compiles `grammars` which are a set of inter-related grammars into a matcher that can be used
    * to apply the grammars to a buffer. The last grammar is presumed to be the main grammar for
    * the mode and the other grammars are for languages which can be nested in the main grammar.
    */
  case class Set (grammars :Seq[Grammar]) {
    val matcher = {
      val compilers = new HashMap[String,Compiler]()
      grammars foreach { g => compilers.put(g.scopeName, new Compiler(compilers, g)) }
      Matcher.first(compilers.get(grammars.last.scopeName).matchers)
    }
    def main :Grammar = grammars.last
  }
  object Set {
    def apply (grammars :Grammar*) :Set = apply(Seq(grammars :_*))
  }

  /** Parses a `tmLanguage` grammar file which should be in NDF format. */
  def parseNDF (file :Path) :Grammar = NDFGrammar.toGrammar(NDF.read(file))
  /** Parses a `tmLanguage` grammar description which should be in NDF format. */
  def parseNDF (in :InputStream) :Grammar = NDFGrammar.toGrammar(NDF.read(in))

  /** Parses a Scaled resource containing a list of `tmLanguage` grammar files in NDF format and
    * creates a set. The last grammar must be the primary grammar. */
  val parseNDFs = (rsrc :Resource) => Grammar.Set(
    rsrc.lines.map(lns => NDFGrammar.toGrammar(NDF.read(lns.toList))))

  /** Parses a `tmLanguage` grammar file which should be in plist XML format. */
  def parsePlist (file :File) :Grammar = PlistGrammar.parse(file)
  /** Parses a `tmLanguage` grammar description, which should be in plist XML format. */
  def parsePlist (in :InputStream) :Grammar = PlistGrammar.parse(in)

  private class Compiler (compilers :HashMap[String,Compiler], grammar :Grammar) {
    val cache = new HashMap[String, List[Matcher]]()
    val incFn = (_ :String) match {
      case "$self" => matchers
      case "$base" => matchers
      case group if (group startsWith "#") => cache.get(group substring 1) match {
        case null => println(s"Unknown include [grammar=${grammar.name}, group=$group]") ; Nil
        case ms   => ms
      }
      case lang => compilers.get(lang) match {
        case null => println(s"Unknown include [grammar=${grammar.name}, lang=$lang]") ; Nil
        case comp => comp.matchers
      }
    }
    grammar.repository foreach { (k, v) => cache.put(k, v.compile(incFn)) }
    lazy val matchers :List[Matcher] = grammar.patterns.flatMap(_.compile(incFn))
  }
}
