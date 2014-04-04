//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import scaled.{Loc, Region}

/** Represents a tagged span of text in a buffer. */
class Span (val start :Loc, val end :Loc, val name :String) extends Region with Comparable[Span] {

  /** Returns true if `this` completely encloses `that`. */
  def encloses (that :Span) = !(that.start < start) && !(that.end > end)

  override def compareTo (that :Span) = {
    val scmp = start.compareTo(that.start)
    // a longer span sorts before a shorter span that starts at the same location
    if (scmp != 0) scmp else -end.compareTo(that.end)
  }

  override def toString = s"${Region.toString(start, end)} = $name"
}
