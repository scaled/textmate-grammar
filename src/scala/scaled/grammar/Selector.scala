//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import scala.annotation.tailrec
import scaled._

/** Matches scopes. */
abstract class Selector {

  /** Returns the depth into the supplied scopes list at which this selector matches the supplied
    * scope list, or -1 if it does not match.
    * @param scopes a set of scopes in innermost to outermost order. */
  def matchDepth (scopes :List[String]) :Int
}

object Selector {

  /** A hook that applies a fn to spans which match a scope selector. */
  abstract class Fn (val selector: Selector) {
    /** Called for every span that matches our `selector`. */
    def apply (buf :Buffer, start :Loc, end :Loc) :Unit
  }

  /** A helper class for using a group of selectors to process a buffer. The canonical use is to use
    * a collection of selector to style mappings (i.e. a TextMate theme) to apply style classes to
    * the approriate regions of the buffer.
    */
  class Processor (sels :List[Fn]) {

    /** Called before applying this processor to any spans on `row` in `buf`. */
    def onBeforeLine (buf :Buffer, row :Int) {
      // nada by default
    }

    /** Applies this processor to the `span` on `row` in `buf`. */
    def apply (buf :Buffer, row :Int, span :Span) {
      @inline @tailrec def maxMatches (sels :List[Fn], depth :Int, matches :List[Fn]) :List[Fn] = {
        // TODO: revamp this to be more efficient
        if (sels.isEmpty) matches
        else {
          val d = sels.head.selector.matchDepth(span.scopes)
          if (d > depth) maxMatches(sels.tail, d, sels.head :: Nil)
          else if (d == depth) maxMatches(sels.tail, d, sels.head :: matches)
          else maxMatches(sels.tail, depth, matches)
        }
      }
      val start = Loc(row, span.start) ; val end = Loc(row, span.end)
      var fns = maxMatches(sels, 0, Nil)
      if (fns.isEmpty) onUnmatched(buf, start, end)
      else while (!fns.isEmpty) {
        fns.head(buf, start, end) ; fns = fns.tail
      }
    }

    /** Called for spans that did not match any of our supplied selectors. This is useful, for
      * example, for clearing styles from unmatched spans which may have previously matched.
      */
    protected def onUnmatched (buf :Buffer, start :Loc, end :Loc) {
    }
  }

  /** Parses a selector description.
    * @see http://manual.macromates.com/en/scope_selectors.html */
  def parse (selstr :String) :Selector = selstr split(",") map(_.trim) match {
    case Array(selstr) => parse0(selstr)
    case selstrs => new Any(List.from(selstrs).map(parse0))
    null
  }

  private def parse0 (selstr :String) :Selector = selstr.indexOf("-") match {
    case -1 => new Path(List.from(selstr.split(" ")).map(_.trim).reverse)
    case ii => new Exclude(parse0(selstr.substring(0, ii)), parse0(selstr.substring(ii+1)))
  }

  private class Any (sels :List[Selector]) extends Selector {
    def matchDepth (scopes :List[String]) = {
      @tailrec @inline def loop (ss :List[Selector], max :Int) :Int =
        if (ss.isEmpty) max
        else loop(ss.tail, math.max(max, ss.head.matchDepth(scopes)))
      loop(sels, -1)
    }
    override def toString = sels.mkString(", ")
  }

  private class Exclude (want :Selector, dontWant :Selector) extends Selector {
    def matchDepth (scopes :List[String]) = {
      val d = want.matchDepth(scopes)
      if (dontWant.matchDepth(scopes) == -1) d else -1
    }
    override def toString = s"$want - $dontWant"
  }

  private class Path (pres :List[String]) extends Selector {
    def matchDepth (scopes :List[String]) = {
      @tailrec @inline def loop (ss :List[String], ps :List[String], depth :Int) :Int =
        if (ps.isEmpty) depth
        else if (ss.isEmpty) -1
        else if (!(ss.head startsWith ps.head)) loop(ss.tail, ps, depth)
        else loop(ss.tail, ps.tail, if (depth == -1) ss.length else depth)
      loop(scopes, pres, -1)
    }
    override def toString = pres.reverse.mkString(" ")
  }
}
