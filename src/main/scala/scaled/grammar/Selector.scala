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
    * @param scopes a set of scopes in outermost to innermost order. */
  def matchDepth (scopes :List[String]) :Int
}

object Selector {

  /** A helper class for using a group of selectors to process a buffer. The canonical use is to use
    * a collection of selector to style mappings (i.e. a TextMate theme) to apply style classes to
    * the approriate regions of the buffer.
    */
  class Processor (sels :List[(Selector, (Buffer,Span) => Unit)]) {
    // TODO: revamp this to be vastly more efficient
    def apply (scopes :List[String], buf :Buffer, span :Span) {
      @inline @tailrec def maxMatches (
        sels :List[(Selector, (Buffer,Span) => Unit)], depth :Int,
        matches :List[(Buffer,Span) => Unit]) :List[(Buffer,Span) => Unit] = {
        if (sels.isEmpty) matches
        else {
          val d = sels.head._1.matchDepth(scopes)
          if (d > depth) maxMatches(sels.tail, d, sels.head._2 :: Nil)
          else if (d == depth) maxMatches(sels.tail, d, sels.head._2 :: matches)
          else maxMatches(sels.tail, depth, matches)
        }
      }
      @inline @tailrec def applyFns (fns :List[(Buffer,Span) => Unit]) {
        if (!fns.isEmpty) { fns.head(buf, span) ; applyFns(fns.tail) }
      }
      applyFns(maxMatches(sels, 0, Nil))
    }
  }

  /** Parses a selector description.
    * @see http://manual.macromates.com/en/scope_selectors.html */
  def parse (selstr :String) :Selector = selstr split(",") map(_.trim) match {
    case Array(selstr) => parse0(selstr)
    case selstrs => new Any(selstrs.map(parse0).toList)
    null
  }

  private def parse0 (selstr :String) :Selector = selstr.indexOf("-") match {
    case -1 => new Path(selstr.split(" ").map(_.trim).toList)
    case ii => new Exclude(parse0(selstr.substring(0, ii)), parse0(selstr.substring(ii+1)))
  }

  private class Any (sels :List[Selector]) extends Selector {
    def matchDepth (scopes :List[String]) = {
      @tailrec @inline def loop (ss :List[Selector], max :Int) :Int =
        if (ss.isEmpty) max
        else loop(ss.tail, math.max(max, ss.head.matchDepth(scopes)))
      loop(sels, -1)
    }
  }

  private class Exclude (want :Selector, dontWant :Selector) extends Selector {
    def matchDepth (scopes :List[String]) = {
      val d = want.matchDepth(scopes)
      if (dontWant.matchDepth(scopes) == -1) d else -1
    }
  }

  private class Path (pres :List[String]) extends Selector {
    def matchDepth (scopes :List[String]) = {
      @tailrec @inline def loop (ss :List[String], ps :List[String], depth :Int) :Int =
        if (ps.isEmpty) depth
        else if (ss.isEmpty) -1
        else if (ss.head startsWith ps.head) loop(ss.tail, ps.tail, depth+1)
        else loop(ss.tail, ps, depth+1)
      loop(scopes, pres, -1)
    }
  }
}
