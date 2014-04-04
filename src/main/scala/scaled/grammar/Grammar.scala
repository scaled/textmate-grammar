//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

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

  /** Compiles `grammars` which are a set of inter-related grammars, and returns the matchers from
    * the last grammar in the list. The last grammar is presumed to be the main grammar for the
    * mode and the other grammars are for languages which can be nested in the main grammar.
    */
  def compile (grammars :Seq[Grammar]) :List[Matcher] = {
    val compilers = MMap[String,Compiler]()
    grammars foreach { g => compilers += (g.scopeName -> new Compiler(compilers, g)) }
    compilers(grammars.last.scopeName).matchers
  }

  private class Compiler (compilers :String => Compiler, grammar :Grammar) {
    val cache = MMap[String, List[Matcher]]()
    lazy val matchers :List[Matcher] = grammar.patterns.flatMap(
      _.compile(compilers(_).matchers, grammar.repository, cache))
  }
}
