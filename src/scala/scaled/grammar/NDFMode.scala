//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._
import scaled.grammar._
import scaled.code.{CodeConfig, Commenter, Indenter}

object NDFConfig extends Config.Defs {
  import CodeConfig._
  import GrammarConfig._

  // map TextMate grammar scopes to Scaled style definitions
  val effacers = List(
    effacer("comment.line", commentStyle),
    effacer("punctuation.line-cont", typeStyle),
    effacer("keyword", keywordStyle)
  )

  // map TextMate grammar scopes to Scaled syntax definitions
  val syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment)
  )

  val grammars = resource("NDF.ndf")(Grammar.parseNDFs)
}

@Major(name="ndf",
       tags=Array("code", "project", "ndf"),
       pats=Array(".*\\.ndf"),
       desc="A major mode for editing Nested Dictionary Format (NDF) files.")
class NDFMode (env :Env) extends GrammarCodeMode(env) {

  override def configDefs = NDFConfig :: super.configDefs
  override def grammars = NDFConfig.grammars.get
  override def effacers = NDFConfig.effacers
  override def syntaxers = NDFConfig.syntaxers

  // HACK: leave indent as-is
  override def computeIndent (row :Int) :Int =
    Indenter.readIndent(buffer, Loc(row, 0))

  override val commenter = new Commenter() {
    override def linePrefix = "#"
  }
}
