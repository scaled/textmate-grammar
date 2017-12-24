//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scaled._

/** Applies a grammar (and any dependent grammars) to an [[RBuffer]] to obtain an initial scoping
  * of all text in the buffer. Then also listens for modifications to the buffer and updates the
  * scopings to accommodate the changes.
  *
  * @param procs a list of processors that will be applied first to the whole buffer, then to any
  * parts of the buffer that are rescoped due to the buffer being edited.
  */
class Scoper (grammar :Grammar, matcher :Matcher, buf :Buffer, procs :List[Selector.Processor]) {

  /** Returns the scope names applied to `loc` in outer- to inner-most order. */
  def scopesAt (loc :Loc) :List[String] = curState(loc.row).scopesAt(loc.col).reverse

  /** Generates a debugging representation of this scoper's matchers.
    * @param expand the names of #include scopes to expand. */
  def showMatchers (expand :Set[String]) :String = topMatcher.show(expand, 0)

  /** Generates a debugging representation of this scoper's scope assignments. */
  def showScopes (row :Int) :Seq[String] = curState(row).showScopes

  /** Re-matches and re-faces the entire buffer. */
  def rethinkBuffer () :Unit = cascadeRethink(0, true)

  /** Connects this scoper to `buf`, using `didInvoke` to batch refacing. */
  def connect (buf :RBuffer, didInvoke :SignalV[String]) :this.type = {
    assert(this.buf eq buf)
    // listen for changes to the buffer and note the region that needs rethinking
    buf.edited.onValue(processEdit)
    // when a fn completes, rethink any changes we noted during edit notifications
    didInvoke.onEmit(processRethinks)
    // compute states for all of the starting rows (TODO: turn this into something that happens
    // lazily the first time a line is made visible...)
    cascadeRethink(0, false)
    this
  }

  override def toString = s"Scoper(${grammar}, $buf)"

  private val topMatcher = matcher
  private val topState = new Matcher.State(List(topMatcher), List(grammar.scopeName))

  private var rethinkStart = Int.MaxValue
  private var rethinkEnd = -1

  private def curState (row :Int) :Matcher.State = buf.lines(row).lineTag(Matcher.NoState)
  private def setState (row :Int, state :Matcher.State) :Unit = buf.setLineTag(row, state)

  private def processEdit (edit :Buffer.Edit) = edit match {
    case Buffer.Insert(start, end) =>
      rethinkStart = math.min(rethinkStart, start.row)
      rethinkEnd = math.max(rethinkEnd, end.row)
    case Buffer.Delete(start, end, _) =>
      rethinkStart = math.min(rethinkStart, start.row)
      rethinkEnd = math.max(rethinkEnd, start.row)
    case Buffer.Transform(start, end, _) =>
      rethinkStart = math.min(rethinkStart, start.row)
      rethinkEnd = math.max(rethinkEnd, end.row)
  }

  private def processRethinks () {
    if (rethinkEnd >= rethinkStart) {
      var row = rethinkStart ; val end = rethinkEnd
      while (row <= end) { setState(row, rethink(row)) ; row += 1 }
      cascadeRethink(row, false)
      rethinkStart = Int.MaxValue
      rethinkEnd = -1
    }
  }

  private def rethink (row :Int) :Matcher.State = {
    // println(s"RETHINK $row ${buf.lines(row)}")
    val pstate = if (row == 0) topState else curState(row-1)
    val state = pstate.continue(buf.lines(row))
    state.apply(procs, buf, row)
    state
  }

  // rethinks row; if end of row state changed, rethinks the next row as well; &c
  private def cascadeRethink (row :Int, force :Boolean) {
    if (row < buf.lines.length) {
      val ostate = curState(row) ; val nstate = rethink(row)
      setState(row, nstate)
      if (ostate == null || force || (ostate nequiv nstate)) cascadeRethink(row+1, force)
    }
  }
}
