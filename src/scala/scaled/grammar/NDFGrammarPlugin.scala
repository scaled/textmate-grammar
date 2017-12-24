//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._
import scaled.code.CodeConfig

@Plugin(tag="textmate-grammar")
class NDFGrammarPlugin extends GrammarPlugin {
  import CodeConfig._

  override def grammars = Map("source.ndf" -> "NDF.ndf")

  override def effacers = List(
    effacer("comment.line", commentStyle),
    effacer("punctuation.line-cont", typeStyle),
    effacer("keyword", keywordStyle)
  )

  override def syntaxers = List(
    syntaxer("comment.line", Syntax.LineComment)
  )
}
