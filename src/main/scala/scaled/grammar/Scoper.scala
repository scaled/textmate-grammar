//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import java.util.TreeSet
import scala.annotation.tailrec
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
  def scopesAt (loc :Loc) :List[String] = topScope.scopesAt(loc)

  /** Applies `proc` to our buffer, matching its selectors against our assigned scope names. */
  def apply (proc :Selector.Processor) :Unit = topScope.apply(proc, Nil)

  /** Generates a debugging representation of this scoper's matchers.
    * @param expand the names of #include scopes to expand. */
  def showMatchers (expand :Set[String]) :String = Matcher.show(matchers, expand)

  /** Generates a debugging representation of this scoper's scope assignments. */
  def showScopes :String = topScope.show("")

  override def toString = s"Scoper($grammars, $buf)"

  private val matchers :List[Matcher] = Grammar.compile(grammars)
  private val topScope :Scope = {
    val spans = new TreeSet[Span.Impl]()
    val top = Span(grammars.last.scopeName, buf.start, buf.end)
    // accumulate all spans in the document into our ordered tree set
    Matcher.applyTo(matchers, spans, buf, top.start, top.end)
    // now turn those scopes into a big tree; see consume scopes for details
    new Scope(top, new Peekerator(spans.iterator))
  }

  // listen for changes to the buffer and keep things up to date
  buf.edited.onValue { _ match {
    case    Buffer.Insert(start, end   ) => topScope.onInsert(start, end)
    case    Buffer.Delete(start, end, _) => topScope.onDelete(start, end)
    case Buffer.Transform(start, end, _) => // TODO
  }}

  // allows to peek at the first span in `iter` without consuming it
  private class Peekerator (iter :Iterator[Span.Impl]) {
    private[this] var cur :Span.Impl = _
    def hasNext :Boolean = iter.hasNext
    def peek :Span.Impl = { if (cur == null) cur = iter.next() ; cur }
    def take () :Span.Impl = { val had = peek ; cur = iter.next() ; had }
  }

  // consumes all the scopes that are completely enclosed by `encl` and returns them as a list; on
  // completion the next span in `iter` will not be enclosed by `encl` (or iter will be empty);
  // note that when a span is turned into a scope, we recursively assign all spans enclosed by
  // *that* span to the newly created scope (this happens in the Scope constructor)
  private def consumeScopes (encl :Span.Impl, iter :Peekerator) :List[Scope] = {
    if (!iter.hasNext || !(encl encloses iter.peek)) Nil
    // note that new Scope(...) will consume all spans enclosed by that scope before returning; so
    // when we resume our consumption, iter will be pointing at the next span *not* enclosed by the
    // scope we just created
    else new Scope(iter.take(), iter) :: consumeScopes(encl, iter)
  }

  private class Scope (var span :Span.Impl, iter :Peekerator) {
    private[this] var scopes = consumeScopes(span, iter).toArray

    // set our span as the parent of our child spans
    { val ss = scopes ; var ii = 0
      while (ii < ss.length) { ss(ii).span.setParent(span) ; ii += 1 }}

    def show (indent :String) :String = {
      val nindent = s"$indent.."
      s"${indent}$span\n${scopes.map(_.show(nindent)).mkString}"
    }

    def scopesAt (loc :Loc) :List[String] = span.name :: findScopesAt(scopes, loc)

    def apply (proc :Selector.Processor, path :List[String]) {
      val spath = path :+ span.name
      proc.apply(spath, buf, span)
      val ss = scopes ; var ii = 0; while (ii < ss.length) { ss(ii).apply(proc, spath) ; ii += 1 }
      // scopes foreach(_.apply(proc, spath))
    }

    def onInsert (start :Loc, end :Loc) {
      val srow = start.row
      procInsert(start, end, srow, end.row-srow, end.col-start.col)
    }

    def onDelete (start :Loc, end :Loc) {
      val erow = end.row
      procDelete(start, end, erow, start.row-erow, start.col-end.col)
    }

    private def shift (pos :Loc, prow :Int, rowΔ :Int, colΔ :Int) :Boolean = {
      val prowΔ = prow - span.srow
      // if the shift point is before our span's row, we only need to shift our row
      if (prowΔ < 0) {
        // if this shift is not multi-row, we can stop processing now;
        // it cannot affect any spans after this one in the buffer
        if (rowΔ == 0) return false
        // println(s"rowShift $span ($prow Δr=$rowΔ Δc=$colΔ)")
        span.shift(prow, rowΔ, colΔ)
      }
      // if this shift is on our row and before our start column, shift our span to accommodate it
      // and shift any same-rowed children
      else if (prowΔ == 0 && pos.col <= span.scol) {
        // println(s"rowColShift $span ($prow Δr=$rowΔ Δc=$colΔ)")
        // if we're column shifting, shift all of our children which are also on this same line
        val ss = scopes ; var ii = 0 ; if (colΔ != 0) while (ii < ss.length &&
          ss(ii).shift(pos, prow, 0, colΔ)) ii += 1
        // then shift ourselves (we have to do this after shifting our children because once we
        // shift our start row, the children's start row will necessarily change, which would break
        // the logic when called on the children)
        span.shift(prow, rowΔ, colΔ)
      }
      true
    }

    private def procInsert (start :Loc, end :Loc, srow :Int, rowΔ :Int, colΔ :Int) :Boolean = {
      // if this insertion is before our span, shift our span
      if (start <= span.start) shift(start, srow, rowΔ, colΔ)
      // if the insertion is inside our span, expand our span and recurse to our children
      else if (start < span.end) {
        // println(s"expand $span ($srow Δr=$rowΔ Δc=$colΔ)")
        span = span.expand(srow, end, rowΔ, colΔ)
        val ss = scopes ; var ii = 0 ; while (ii < ss.length &&
          ss(ii).procInsert(start, end, srow, rowΔ, colΔ)) ii += 1
      }
      true
    }

    // returns one of: -1 = delete me (and keep going), 0 = keep going, 1 = stop processing
    private def procDelete (start :Loc, end :Loc, erow :Int, rowΔ :Int, colΔ :Int) :Int = {
      val _start = span.start ; val _end = span.end
      // println(s"procDelete($start, $end, $erow, $rowΔ, $colΔ) on $span")
      // if this deletion is entirely before this span, shift our span
      if (end <= _start) if (shift(end, erow, rowΔ, colΔ)) 0 else 1
      // if this deletion is entirely after this span, we can ignore it
      else if (_end <= start) 0
      // otherwise we need to trim or delete this span
      else {
        // intersect the deletion bounds with our span bounds
        val tstart = start greater _start ; val tend = _end lesser end
        // if the deletion encloses us completely, we (and all of our children) go away
        if (tstart == _start && tend == _end) -1
        // otherwise we recursively process our children, then shrink oureslves
        else {
          val ss = scopes ; var ii = 0 ; var dstart = -1 ; var dend = -1
          while (ii < ss.length) ss(ii).procDelete(start, end, erow, rowΔ, colΔ) match {
            case  1 => ii = ss.length // stop processing
            case -1 => if (dstart == -1) dstart = ii ; dend = math.max(dend, ii) ; ii += 1
            case  _ => ii += 1 // keep going
          }
          if (dstart >= 0) scopes = splice(scopes, dstart, ss.length-dend-1)
          span = span.shrink(tstart, tend)
          // if the deletion start preceded our own start, we also need to shift
          if (start < _start) {
            val srow = start.row
            shift(start, srow, _start.row-srow, _start.col-start.col)
          }
          0
        }
      }
    }

    private def splice (scopes :Array[Scope], head :Int, tail :Int) = {
      val nscopes = new Array[Scope](head+tail)
      if (head > 0) System.arraycopy(scopes, 0, nscopes, 0, head)
      if (tail > 0) System.arraycopy(scopes, scopes.length-tail, nscopes, head, tail)
      nscopes
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
}
