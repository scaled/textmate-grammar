//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import java.util.TreeSet
import java.util.regex.{Pattern => JPattern, Matcher => JMatcher}
import scala.annotation.tailrec
import scaled._

/** A matcher is created from a grammar rule. The grammar rules are compiled into a tree of
  * matchers which efficiently apply those rules to a buffer, tagging characters with styles.
  */
abstract class Matcher {

  /** Applies this matcher to `buf` starting at `start`.
    * @param spans a buffer to which named spans are added.
    * @param buf the buffer to which we're applying this matcher.
    * @param start the location in the buffer at which we're seeking a match.
    * @return `start` if this matcher did not match, or the position at the end of this matcher's
    * match, if it did match.
    */
  def apply (spans :TreeSet[Span], buf :Buffer, start :Loc) :Loc
}

object Matcher {

  /** Applies `matchers` to `buf` starting at `start` and stopping when `end` is reached or `atEnd`
    * returns true (whichever happens first).
    * @return the location at which matching stopped.
    */
  def applyTo (matchers :List[Matcher], spans :TreeSet[Span], buf :Buffer, start :Loc, end :Loc,
               atEnd :(Buffer, Loc) => Boolean = (_, _) => false) :Loc = {
    @tailrec @inline def applyFirst (ms :List[Matcher], loc :Loc) :Loc = if (ms.isEmpty) loc else {
      val nloc = ms.head.apply(spans, buf, loc)
      if (nloc == loc) applyFirst(ms.tail, loc)
      else nloc
    }
    var loc = start
    while (loc < end && !atEnd(buf, loc)) {
      // if we're at the end of a line, move to the start of the next line
      // TODO: skip whitespace too?
      if (loc == buf.lineEnd(loc)) loc = buf.forward(loc, 1)
      else {
        val nloc = applyFirst(matchers, loc)
        // if no rules matched, advance to the next character and try again
        if (nloc == loc) loc = buf.forward(loc, 1)
        // otherwise advance to the end of the matched region and continue
        else loc = nloc
      }
    }
    loc
  }

  /** Handles matching a pattern and applying a set of captures. */
  class Pattern (regexp :String, captures :List[(Int,String)]) {
    private[this] val p = JPattern.compile(regexp)
    private[this] val m = p.matcher("")
    private[this] val fullLine = regexp startsWith "^"

    var matched = false
    @inline private def note (matched :Boolean) = { this.matched = matched ; matched }

    def apply (buf :Buffer, loc :Loc) :Boolean = note {
      if (fullLine && loc.col != 0) false
      else {
        val line = buf.line(loc)
        m.reset(line)
        m.region(loc.col, line.length)
        m.lookingAt
      }
    }

    def capture (spans :TreeSet[Span], loc :Loc) :Loc = if (!matched) loc else {
      @tailrec @inline def loop (captures :List[(Int,String)]) :Unit = if (!captures.isEmpty) {
        val group = captures.head._1 ; val start = m.start(group)
        if (start >= 0) spans add new Span(
          loc.atCol(start), loc.atCol(m.end(group)), captures.head._2)
        loop(captures.tail)
      }
      loop(captures)
      loc.atCol(m.end)
    }

    override def toString = m.toString
  }

  /** A matcher that matches a regexp in a single line. */
  class Single (pattern :Pattern) extends Matcher {
    def apply (spans :TreeSet[Span], buf :Buffer, start :Loc) =
      if (!pattern.apply(buf, start)) start
      else pattern.capture(spans, start)
  }

  /** A matcher that matches a begin regexp, then applies a set of nested matchers until an end
    * regexp is seen (potentially on a new line). */
  class Multi (begin :Pattern, end :Pattern, name :Option[String], contentName :Option[String],
               contentMatchers :List[Matcher]) extends Matcher {

    private[this] val atEnd = (buf :Buffer, loc :Loc) => end.apply(buf, loc)

    override def apply (spans :TreeSet[Span], buf :Buffer, start :Loc) = {
      if (!begin.apply(buf, start)) start
      else {
        val contentStart = begin.capture(spans, start)
        val contentEnd = applyTo(contentMatchers, spans, buf, contentStart, buf.end, atEnd)
        contentName.map(nm => spans add new Span(contentStart, contentEnd, nm))
        if (!end.matched) contentEnd
        else {
          val endEnd = end.capture(spans, contentEnd)
          name.map(nm => spans add new Span(start, endEnd, nm))
          endEnd
        }
      }
    }
  }
}
