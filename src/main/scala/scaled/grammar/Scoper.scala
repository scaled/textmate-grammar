//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scaled._

/** Applies a set of grammars to an [[RBuffer]] to obtain an initial scoping of all text in the
  * buffer. Then also listens for modifications to the buffer and updates the scopings to
  * accommodate the changes.
  *
  * The last grammar in the list is considered to be the main grammar, the other grammars are
  * presumed to be referenced by the main grammar as sub-languages.
  *
  * @param procs a list of processors that will be applied first to the whole buffer, then to any
  * parts of the buffer that are rescoped due to the buffer being edited.
  */
class Scoper (grammars :Seq[Grammar], buf :RBuffer, procs :List[Selector.Processor]) {

  /** Returns the scope names applied to `loc` in outer- to inner-most order. */
  def scopesAt (loc :Loc) :List[String] = states(loc.row).scopesAt(loc.col).reverse

  /** Generates a debugging representation of this scoper's matchers.
    * @param expand the names of #include scopes to expand. */
  def showMatchers (expand :Set[String]) :String = topMatcher.show(expand, 0)

  /** Generates a debugging representation of this scoper's scope assignments. */
  def showScopes (row :Int) :String = states(row).showScopes

  /** Applies the registered processors to the entire buffer. */
  def applyProcs () {
    var row = 0; while (row < states.length) { states(row).apply(procs, buf, row) ; row += 1 }
  }

  override def toString = s"Scoper($grammars, $buf)"

  private val topMatcher = Grammar.compile(grammars)
  private val topState = new Matcher.State(List(topMatcher), List(grammars.last.scopeName))
  private val states = new ArrayBuffer[Matcher.State](buf.lines.size)

  // compute states for all of the starting rows
  { val ll = buf.lines.size ; var ii = 0 ; while (ii < ll) { states += rethink(ii) ; ii += 1 }}

  // listen for changes to the buffer and keep things up to date
  buf.edited.onValue { _ match {
    case Buffer.Insert(start, end) => // rethink the start row, insert (and think) any new rows
      val srow = start.row ; val erow = end.row
      states(srow) = rethink(srow)
      var row = srow+1 ; while (row <= erow) { states.insert(row, rethink(row)) ; row += 1 }

    case Buffer.Delete(start, end, _) => // rethink the start row, delete nixed rows
      val srow = start.row ; val erow = end.row
      states(srow) = rethink(srow)
      if (erow > srow) states.remove(srow+1, erow-srow)

    case Buffer.Transform(start, end, _) => // rethink all the transformed rows
      val erow = end.row ; var row = start.row
      while (row <= erow) { states(row) = rethink(row) ; row += 1 }
  }}

  private def rethink (row :Int) :Matcher.State = {
    // println(s"RETHINK $row ${buf.lines(row)}")
    val pstate = if (row == 0) topState else states(row-1)
    val state = pstate.continue(buf.lines(row))
    state.apply(procs, buf, row)
    state
  }
}
