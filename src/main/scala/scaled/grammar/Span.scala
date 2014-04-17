//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import scaled.{Loc, Region}

/** Represents a tagged span of text in a buffer. */
abstract class Span (val name :Option[String], val matcher :Option[Matcher])
    extends Region with Comparable[Span] {

  /** Returns true if `this` completely encloses `that`. */
  def encloses (that :Region) = !(that.start < start) && !(that.end > end)

  override def compareTo (that :Span) = {
    val scmp = start.compareTo(that.start)
    // a longer span sorts before a shorter span that starts at the same location
    if (scmp != 0) scmp else -end.compareTo(that.end)
  }

  override def toString = s"${Region.toString(start, end)} = ${name getOrElse "<>"}"
}

object Span {

  /** Creates a span with the specified name and bounds.
    * @param matcher the matcher to use when rebuilding scopes due to an edit inside this span.
    */
  def apply (name :Option[String], matcher :Option[Matcher], start :Loc, end :Loc) :Impl = {
    if (start.row == end.row) new Single(name, matcher, start, end.col - start.col)
    else new Multi(name, matcher, start, end)
  }

  private val Zero = new Single(None, None, Loc.Zero, 0) {
    override def srow = 0
  }

  abstract class Impl (nm :Option[String], m :Option[Matcher], _start :Loc) extends Span(nm, m) {

    private[this] var _parent :Impl = Zero
    private[this] var _srow :Int = _start.row
    protected     var _scol :Int = _start.col

    override final def start :Loc = Loc(srow, scol)
    override final def end   :Loc = Loc(erow, ecol)
    override def toString = super.toString + s" (+${_srow})"

    def srow :Int = _parent.srow + _srow
    def scol :Int = _scol
    def erow :Int
    def ecol :Int

    def setParent (parent :Impl) :Impl = {
      _parent = parent
      _srow -= parent.start.row
      this
    }
    def parent :Impl = _parent

    def shift (srow :Int, rowΔ :Int, colΔ :Int) {
      if (this.srow == srow) _scol += colΔ
      _srow += rowΔ
    }
    def expand (srow :Int, end :Loc, rowΔ :Int, colΔ :Int) :Impl
    def shrink (tstart :Loc, tend :Loc) :Impl

    protected def shiftRow (rowΔ :Int) :Unit = _srow += rowΔ
  }

  private class Single (nm :Option[String], m :Option[Matcher], _start :Loc, length :Int)
      extends Impl(nm, m, _start) {

    private var _length :Int = length

    override final def erow :Int = srow
    override final def ecol :Int = _scol+_length

    final def expand (srow :Int, end :Loc, rowΔ :Int, colΔ :Int) :Impl = {
      // if we're expanding into a multirow span, return a new Multi
      if (rowΔ > 0) new Multi(name, matcher, this.start, end).setParent(parent)
      else {
        _length += colΔ
        this
      }
    }
    final def shrink (tstart :Loc, tend :Loc) :Impl = {
      _length += tstart.col - tend.col
      this
    }
  }

  private class Multi (nm :Option[String], m :Option[Matcher], _start :Loc, _end :Loc)
      extends Impl(nm, m, _start) {

    private[this] var _rows :Int = _end.row-_start.row
    private[this] var _ecol :Int = _end.col

    override final def erow :Int = srow+_rows
    override final def ecol :Int = _ecol

    final def expand (srow :Int, end :Loc, rowΔ :Int, colΔ :Int) :Impl = {
      if (srow == erow) _ecol += colΔ // if our final row is expanding, adjust our end column
      _rows += rowΔ
      this
    }
    final def shrink (tstart :Loc, tend :Loc) :Impl = {
      val nrows = _rows - (tend.row - tstart.row)
      val necol = _ecol - (if (tend.row == erow) (tend.col - tstart.col) else 0)
      // if we shrunk down to a single line span, return a new Single
      if (nrows == 0) new Single(name, matcher, start, necol-scol).setParent(parent)
      else {
        _rows = nrows
        _ecol = necol
        this
      }
    }
  }
}
