//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import java.util.TreeSet
import scaled._

/** Applies a set of grammars to an [[RBuffer]] to obtain an initial naming of all text in the
  * buffer. Then also listens for modifications to the buffer and updates the namings to
  * accommodate the changes.
  *
  * The last grammar in the list is considered to be the main grammar, the other grammars are
  * presumed to be referenced by the main grammar as sub-languages.
  */
class Scoper (grammars :Seq[Grammar], buf :RBuffer) {
  import scala.collection.convert.WrapAsScala._

  /** Returns the scope names applied to `loc` in outer- to inner-most order. */
  def scopesAt (loc :Loc) :List[String] = findScopesAt(scopes, loc)

  /** Applies `proc` to our buffer, matching its selectors against our assigned scope names. */
  def apply (proc :Selector.Processor) {
    scopes foreach { _.apply(proc, Nil) }
  }

  //
  // implementation details!

  private val matchers = Grammar.compile(grammars)
  private val scopes = buildScopes(buf.start, buf.end)

  // TODO: listen for changes to the buffer and keep things up to date

  private def buildScopes (start :Loc, end :Loc) = {
    val spans = new TreeSet[Span]()
    // accumulate all spans in the document into our ordered tree set
    Matcher.applyTo(matchers, spans, buf, start, end)
    // now turn those scopes into a big tree; see consume scopes for details
    consumeScopes(new Span(start, end, ""), new Peekerator(spans.iterator)).toArray
  }

  // allows to peek at the first span in `iter` without consuming it
  private class Peekerator (iter :Iterator[Span]) {
    private[this] var cur :Span = _
    def hasNext :Boolean = iter.hasNext
    def peek :Span = { if (cur == null) cur = iter.next() ; cur }
    def take () :Span = { val had = peek ; cur = iter.next() ; had }
  }

  // consumes all the scopes that are completely enclosed by `encl` and returns them as a list; on
  // completion the next span in `iter` will not be enclosed by `encl` (or iter will be empty);
  // note that when a span is turned into a scope, we recursively assign all spans enclosed by
  // *that* span to the newly created scope (this happens in the Scope constructor)
  private def consumeScopes (encl :Span, iter :Peekerator) :List[Scope] = {
    if (!iter.hasNext || !(encl encloses iter.peek)) Nil
    // note that new Scope(...) will consume all spans enclosed by that scope before returning; so
    // when we resume our consumption, iter will be pointing at the next span *not* enclosed by the
    // scope we just created
    else new Scope(iter.take(), iter) :: consumeScopes(encl, iter)
  }

  private class Scope (val span :Span, iter :Peekerator) {
    private[this] val scopes = consumeScopes(span, iter).toArray
    def toString (indent :String) :String = {
      val nindent = s"$indent.."
      s"${indent}$span\n${scopes.map(_.toString(nindent)).mkString}"
    }

    def scopesAt (loc :Loc) :List[String] = span.name :: findScopesAt(scopes, loc)

    def apply (proc :Selector.Processor, path :List[String]) {
      val spath = path :+ span.name
      proc.apply(spath, buf, span)
      scopes foreach(_.apply(proc, spath))
    }
  }

  private def findScopesAt (scopes :Array[Scope], loc :Loc) :List[String] = find(scopes, loc) match {
    case null  => Nil
    case scope => scope.scopesAt(loc)
  }

  private def find (scopes :Array[Scope], loc :Loc) :Scope = {
    var low = 0 ; var high = scopes.size-1
    while (low <= high) {
      val mid = (low + high) >>> 1
      val midR = scopes(mid)
      val midS = midR.span.start
      if (midS < loc) low = mid + 1
      else if (midS > loc) high = mid - 1
      else return midR
    }
    if (high < 0) null
    else {
      val highR = scopes(high)
      if (loc < highR.span.end) highR else null
    }
  }

  override def toString = toString(Set())

  /** Generates a debugging representation of this scoper.
    * @param expand the names of #include scopes to expand.
    */
  def toString (expand :Set[String]) = ("MATCHERS:" + Matcher.toString(matchers, expand) + "\n" +
    "SCOPES:\n" + scopes.map(_.toString("")).mkString)
}
