//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._
import scaled.major.CodeMode

object GrammarCodeConfig extends Config.Defs {

  /** An alias for a (`Selector`, style fn) pair. */
  type Effacer = (Selector, (Buffer,Span) => Unit)

  /** Compiles `selector` into a TextMate grammar selector and pairs it with a function that applies
    * `cssClass` to buffer spans matched by the selector. */
  def effacer (selector :String, cssClass :String) :Effacer =
    (Selector.parse(selector), (buf :Buffer, span :Span) => {
      // println(s"Applying $cssClass to $span")
      buf.updateStyles(_ - codeP + cssClass, span)
    })

  /** A predicate we use to strip `code` styles from a line before restyling it. */
  private val codeP = (style :String) => style startsWith "code"
}

/** Extends [[CodeMode]] with support for using TextMate grammars for code highlighting. Code major
  * modes which intend to use TextMate grammars for code highlighting and other purposes should
  * extend this class rather than [[CodeMode]].
  */
abstract class GrammarCodeMode (env :Env) extends CodeMode(env) {
  import GrammarCodeConfig._

  /** Returns the grammars used by this mode. */
  protected def grammars :Seq[Grammar]

  /** Returns the effacers used to colorize code for this mode. Defaults to the empty list, which
    * indicates that the mode does not desire to colorize. */
  protected def effacers :List[Effacer] = Nil

  /** Handles applying the grammars to the buffer and computing scopes. */
  val scoper = {
    val procs = if (effacers.isEmpty) Nil else List(new Selector.Processor(effacers))
    new Scoper(grammars, view.buffer, syntax, procs)
  }

  override def configDefs = GrammarCodeConfig :: super.configDefs
  override def keymap = super.keymap ++ Seq(
    "M-A-p" -> "show-syntax" // TODO: also M-PI?
  )
  override def dispose () {} // TODO: remove all colorizations?

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
