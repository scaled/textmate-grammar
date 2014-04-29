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

  /** Returns the grammars used by this mode. */
  protected def grammars :Seq[Grammar]

  /** Returns the effacers used to colorize code for this mode. Defaults to the empty list, which
    * indicates that the mode does not desire to colorize. */
  protected def effacers :List[Selector.Fn] = Nil

  /** Handles applying the grammars to the buffer and computing scopes. */
  val scoper = {
    val procs = if (effacers.isEmpty) Nil else List(new Selector.Processor(effacers) {
      override protected def onUnmatched (buf :Buffer, start :Loc, end :Loc) {
        buf.updateStyles(_ - textP, start, end) // clear any text styles
      }
    })
    new Scoper(grammars, buffer, procs)
  }

  override def configDefs = GrammarConfig :: super.configDefs
  override def keymap = super.keymap ++ Seq(
    "M-A-p" -> "show-syntax" // TODO: also M-PI?
  )

  @Fn("Displays the TextMate syntax scopes at the point.")
  def showSyntax () {
    val ss = scoper.scopesAt(view.point())
    view.popup() = Popup(if (ss.isEmpty) List("No scopes.") else ss, Popup.UpRight(view.point()))
  }

  @Fn("Refreshes the colorization of the entire buffer.")
  def refaceBuffer () {
    scoper.applyProcs()
  }
}
