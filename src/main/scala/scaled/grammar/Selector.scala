//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import scala.annotation.tailrec
import scaled._

/** Matches scopes. */
abstract class Selector {

  /** Returns true if this selector matches the supplied scopes.
    * @param scopes a set of scopes in outermost to innermost order. */
  def matches (scopes :List[String]) :Boolean
}

object Selector {

  /** A helper class for using a group of selectors to process a buffer. The canonical use is to use
    * a collection of selector to style mappings (i.e. a TextMate theme) to apply style classes to
    * the approriate regions of the buffer.
    */
  class Processor (sels :Map[Selector, (Buffer,Span) => Unit]) {
    // TODO: revamp this to be vastly more efficient
    def apply (scopes :List[String], buf :Buffer, span :Span) {
      sels foreach { case (sel, fn) => if (sel.matches(scopes)) fn(buf, span) }
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
    def matches (scopes :List[String]) = {
      @tailrec @inline def loop (ss :List[Selector]) :Boolean =
        if (ss.isEmpty) false
        else if (ss.head.matches(scopes)) true
        else loop(ss.tail)
      loop(sels)
    }
  }

  private class Exclude (want :Selector, dontWant :Selector) extends Selector {
    def matches (scopes :List[String]) = want.matches(scopes) && !dontWant.matches(scopes)
  }

  private class Path (pres :List[String]) extends Selector {
    def matches (scopes :List[String]) = {
      @tailrec @inline def loop (ss :List[String], ps :List[String]) :Boolean =
        if (ps.isEmpty) true
        else if (ss.isEmpty) false
        else if (ss.head startsWith ps.head) loop(ss.tail, ps.tail)
        else loop(ss.tail, ps)
      loop(scopes, pres)
    }
  }
}
