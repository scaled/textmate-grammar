//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._
import scaled.major.TextMode

/** Extends [[TextMode]] with support for using TextMate grammars for code highlighting.
  * Non-code major modes which intend to use TextMate grammars for code highlighting and other
  * purposes should extend this class rather than [[TextMode]].
  */
abstract class GrammarTextMode (env :Env) extends TextMode(env) {
  import GrammarConfig._

  /** Returns the grammar set used by this mode. */
  protected def grammars :Grammar.Set

  /** Returns the effacers used to colorize code for this mode. Defaults to the empty list, which
    * indicates that the mode does not desire to colorize. */
  protected def effacers :List[Selector.Fn] = Nil

  /** Handles applying the grammars to the buffer and computing scopes. */
  val scoper = {
    val procs = if (effacers.isEmpty) Nil else List(new Selector.Processor(effacers) {
      override def onBeforeLine (buf :Buffer, row :Int) { // clear any text styles
        val start = buf.lineStart(row) ; val end = buf.lineEnd(row)
        if (start != end) buf.removeTags(classOf[String], textP, start, end)
      }
    })
    new Scoper(grammars, buffer, procs)
  }

  override def configDefs = GrammarConfig :: super.configDefs
  override def keymap = super.keymap.
    bind("show-syntax", "M-A-p"); // TODO: also M-PI?

  @Fn("Displays the TextMate syntax scopes at the point.")
  def showSyntax () {
    val ss = scoper.scopesAt(view.point())
    val text = if (ss.isEmpty) List("No scopes.") else ss
    view.popup() = Popup.text(text, Popup.UpRight(view.point()))
  }

  @Fn("Refreshes the colorization of the entire buffer.")
  def refaceBuffer () {
    scoper.applyProcs()
  }
}
