//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._
import scaled.code.CodeMode
import scaled.util.Errors

/** Extends [[CodeMode]] with support for using TextMate grammars for code highlighting. Code major
  * modes which intend to use TextMate grammars for code highlighting and other purposes should
  * extend this class rather than [[CodeMode]].
  */
abstract class GrammarCodeMode (env :Env) extends CodeMode(env) {

  /** The TextMate scope of the language grammar for this mode. */
  def langScope :String

  /** Handles applying the grammars to the buffer and computing scopes. */
  val scoper = env.msvc.service[GrammarService].scoper(buffer, langScope, mkProcs).
    getOrElse(throw Errors.feedback(s"No grammar available for '$langScope'")).
    connect(buffer, disp.didInvoke)

  override def keymap = super.keymap.
    bind("show-scopes", "M-A-p") // TODO: also M-PI?

  @Fn("Displays the TextMate syntax scopes at the point.")
  def showScopes () {
    val ss = scoper.scopesAt(view.point())
    val text = if (ss.isEmpty) List("No scopes.") else ss
    view.popup() = Popup.text(text, Popup.UpRight(view.point()))
  }

  @Fn("Refreshes the colorization of the entire buffer.")
  def refaceBuffer () :Unit = scoper.rethinkBuffer()

  private def mkProcs (plugin :GrammarPlugin) = {
    val procs = List.builder[Selector.Processor]()
    if (!plugin.effacers.isEmpty) procs += new Selector.Processor(plugin.effacers) {
      override def onBeforeLine (buf :Buffer, row :Int) { // clear any code styles
        val start = buf.lineStart(row) ; val end = buf.lineEnd(row)
        if (start != end) buf.removeTags(classOf[String], (_ :String) startsWith "code", start, end)
      }
    }
    if (!plugin.syntaxers.isEmpty) procs += new Selector.Processor(plugin.syntaxers) {
      override protected def onUnmatched (buf :Buffer, start :Loc, end :Loc) {
        buf.setSyntax(Syntax.Default, start, end) // reset syntax
      }
    }
    procs.build()
  }
}
