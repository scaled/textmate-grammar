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
  def apply (spans :TreeSet[Span.Impl], buf :Buffer, start :Loc, end :Loc) :Loc

  /** Converts this matcher to a string for debugging purposes. */
  def show (expand :Set[String], depth :Int) :String

  protected def nest (depth :Int, msg :String) = ("." * depth) + msg
}

object Matcher {

  /** Applies `matchers` to `buf` starting at `start` and stopping when `end` is reached or `atEnd`
    * returns true (whichever happens first).
    * @return the location at which matching stopped.
    */
  def applyTo (matchers :List[Matcher], spans :TreeSet[Span.Impl], buf :Buffer, start :Loc, end :Loc,
               atEnd :(Buffer, Loc, Loc) => Boolean = (_, _, _) => false) :Loc = {
    var loc = start
    while (loc < end && !atEnd(buf, loc, end)) {
      // if we're at the end of a line, move to the start of the next line
      // TODO: skip whitespace too?
      if (loc == buf.lineEnd(loc)) loc = buf.forward(loc, 1)
      else {
        val nloc = applyFirst(matchers, spans, buf, loc, end)
        // if no rules matched, advance to the next character and try again
        if (nloc == loc) loc = buf.forward(loc, 1)
        // otherwise advance to the end of the matched region and continue
        else loc = nloc
      }
    }
    loc
  }

  /** Converts `matchers` to a string for debugging purposes. */
  def show (matchers :List[Matcher], expand :Set[String], depth :Int = 0) =
    if (matchers.isEmpty) "" else matchers.map(_.show(expand, depth)).mkString("\n", "\n", "")

  @tailrec private def applyFirst (ms :List[Matcher], spans :TreeSet[Span.Impl], buf :Buffer,
                                   start :Loc, end :Loc) :Loc = if (ms.isEmpty) start else {
    val nloc = ms.head.apply(spans, buf, start, end)
    if (nloc == start) applyFirst(ms.tail, spans, buf, start, end)
    else nloc
  }

  def pattern (regexp :String, captures :List[(Int,String)]) :Pattern = try {
    new Pattern(regexp, JPattern.compile(regexp), captures)
  } catch {
    case e :Exception =>
      println(s"Error compiling '$regexp': ${e.getMessage}")
      new Pattern("NOMATCH", JPattern.compile("NOMATCH"), captures)
  }

  /** Handles matching a pattern and applying a set of captures. */
  class Pattern (regexp :String, p :JPattern, captures :List[(Int,String)]) {
    // we use transparent bounds so that things like word boundary detection works properly even
    // when we restrict a pattern to a sub-region of a string
    private[this] val m = p.matcher("").useTransparentBounds(true)
    private[this] val fullLine = regexp startsWith "^"

    var matched = false
    @inline private def note (matched :Boolean) = { this.matched = matched ; matched }

    def apply (buf :Buffer, loc :Loc, end :Loc) :Boolean = note {
      if (fullLine && loc.col != 0) false
      else {
        val line = buf.line(loc)
        m.reset(line)
        m.region(loc.col, line.length)
        m.lookingAt
      }
    }

    def capture (spans :TreeSet[Span.Impl], loc :Loc) :Loc = if (!matched) loc else {
      @tailrec @inline def loop (captures :List[(Int,String)]) :Unit = if (!captures.isEmpty) {
        val group = captures.head._1 ; val name = captures.head._2
        try {
          val start = m.start(group)
          if (start >= 0) spans add Span(name, loc.atCol(start), loc.atCol(m.end(group)))
        } catch {
          case e :Exception =>
            println(s"Capture failure [regexp=$regexp, group=$group, name=$name]: ${e.getMessage}")
        }
        loop(captures.tail)
      }
      loop(captures)
      loc.atCol(m.end)
    }

    override def toString = s"${p.toString}(${captures.map(_._1).mkString})"
  }

  class Deferred (group :String, incFn :String => List[Matcher]) extends Matcher {
    def apply (spans :TreeSet[Span.Impl], buf :Buffer, start :Loc, end :Loc) =
      applyFirst(incFn(group), spans, buf, start, end)

    def show (expand :Set[String], depth :Int) =
      nest(depth, s"Deferred($group)") + Matcher.show(
        if (expand(group)) incFn(group) else List(), expand - group, depth+1)
  }

  /** A matcher that matches a regexp in a single line. */
  class Single (pattern :Pattern) extends Matcher {
    def apply (spans :TreeSet[Span.Impl], buf :Buffer, start :Loc, end :Loc) =
      if (!pattern.apply(buf, start, end)) start
      else pattern.capture(spans, start)
    def show (expand :Set[String], depth :Int) = nest(depth, s"Single($pattern)")
  }

  /** A matcher that matches a begin regexp, then applies a set of nested matchers until an end
    * regexp is seen (potentially on a new line). */
  class Multi (open :Pattern, close :Pattern, name :Option[String], contentName :Option[String],
               contentMatchers :List[Matcher]) extends Matcher {

    private[this] val atEnd = (buf :Buffer, start :Loc, end :Loc) => close.apply(buf, start, end)

    override def apply (spans :TreeSet[Span.Impl], buf :Buffer, start :Loc, end :Loc) = {
      if (!open.apply(buf, start, end)) start
      else {
        val contentStart = open.capture(spans, start)
        val contentEnd = applyTo(contentMatchers, spans, buf, contentStart, buf.end, atEnd)
        contentName.map(nm => spans add Span(nm, contentStart, contentEnd))
        if (!close.matched) contentEnd
        else {
          val endEnd = close.capture(spans, contentEnd)
          name.map(nm => spans add Span(nm, start, endEnd))
          endEnd
        }
      }
    }

    def show (expand :Set[String], depth :Int) =
      nest(depth, s"Multi($open, $close, $name, $contentName)") + Matcher.show(
        contentMatchers, expand, depth+1)
  }
}
