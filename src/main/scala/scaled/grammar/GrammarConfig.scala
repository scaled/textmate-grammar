//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._

/** Configuration for [[GrammarCodeMode]] and [[GrammarTextMode]]. */
object GrammarConfig extends Config.Defs {

  /** Compiles `selector` into a TextMate grammar selector and pairs it with a function that applies
    * `cssClass` to buffer spans matched by the selector. */
  def effacer (selector :String, cssClass :String) :Selector.Fn =
    new Selector.Fn(Selector.parse(selector)) {
      def apply (buf :Buffer, start :Loc, end :Loc) {
        buf.removeTags(classOf[String], codeP, start, end)
        // println(s"Applying $cssClass to $span")
        buf.addStyle(cssClass, start, end)
      }
      override def toString =  s"'$selector' => $cssClass"
    }

  /** Compiles `selector` into a TextMate grammar selector and pairs it with a function that
    * applies `syntax` to buffer spans matched by the selector. */
  def syntaxer (selector :String, syntax :Syntax) :Selector.Fn =
    new Selector.Fn(Selector.parse(selector)) {
      def apply (buf :Buffer, start :Loc, end :Loc) :Unit = buf.setSyntax(syntax, start, end)
      override def toString =  s"'$selector' => $syntax"
    }

  /** A predicate we use to strip `code` styles from a line before restyling it. */
  val codeP = (style :String) => style startsWith "code"

  /** A predicate we use to strip `text` styles from a line before restyling it. */
  val textP = (style :String) => style startsWith "text"
}
