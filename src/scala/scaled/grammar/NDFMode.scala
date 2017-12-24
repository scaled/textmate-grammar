//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._
import scaled.code.{Commenter, Indenter}

@Major(name="ndf",
       tags=Array("code", "project", "ndf"),
       pats=Array(".*\\.ndf"),
       desc="A major mode for editing Nested Dictionary Format (NDF) files.")
class NDFMode (env :Env) extends GrammarCodeMode(env) {

  override def langScope = "source.ndf"

  // HACK: leave indent as-is
  override def computeIndent (row :Int) :Int = Indenter.readIndent(buffer, Loc(row, 0))

  override val commenter = new Commenter() {
    override def linePrefix = "#"
  }
}
