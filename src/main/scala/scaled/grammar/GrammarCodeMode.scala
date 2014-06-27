//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._
import scaled.code.CodeMode

/** Extends [[CodeMode]] with support for using TextMate grammars for code highlighting. Code major
  * modes which intend to use TextMate grammars for code highlighting and other purposes should
  * extend this class rather than [[CodeMode]].
  */
abstract class GrammarCodeMode (env :Env) extends CodeMode(env) {
  import GrammarConfig._

  /** Returns the grammars used by this mode. */
  def grammars :Seq[Grammar]

  /** Returns the effacers used to colorize code for this mode. Defaults to the empty list, which
    * indicates that the mode does not desire to colorize. */
  def effacers :List[Selector.Fn] = Nil

  /** Returns the syntaxers used to assign syntax for this mode. Defaults to the empty list, which
    * indicates that the mode does not desire to syntax. */
  def syntaxers :List[Selector.Fn] = Nil

  /** Handles applying the grammars to the buffer and computing scopes. */
  val scoper = {
    val procs = List.newBuilder[Selector.Processor]
    if (!effacers.isEmpty) procs += new Selector.Processor(effacers) {
      override protected def onUnmatched (buf :Buffer, start :Loc, end :Loc) {
        buf.removeTags(classOf[String], codeP, start, end) // clear any code styles
      }
    }
    if (!syntaxers.isEmpty) procs += new Selector.Processor(syntaxers) {
      override protected def onUnmatched (buf :Buffer, start :Loc, end :Loc) {
        buf.setSyntax(Syntax.Default, start, end) // reset syntax
      }
    }
    new Scoper(grammars, buffer, procs.result)
  }

  override def configDefs = GrammarConfig :: super.configDefs
  override def keymap = super.keymap ++ Seq(
    "M-A-p" -> "show-scopes" // TODO: also M-PI?
  )

  @Fn("Displays the TextMate syntax scopes at the point.")
  def showScopes () {
    val ss = scoper.scopesAt(view.point())
    view.popup() = Popup(if (ss.isEmpty) List("No scopes.") else ss, Popup.UpRight(view.point()))
  }

  @Fn("Refreshes the colorization of the entire buffer.")
  def refaceBuffer () {
    scoper.applyProcs()
  }
}
