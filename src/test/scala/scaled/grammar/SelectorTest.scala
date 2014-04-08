//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import org.junit.Assert._
import org.junit._

class SelectorTest {

  @Test def testMatchDepth () {
    val scopes = List("comment.block.documentation.javadoc",
                      "meta.documentation.comment.javadoc",
                      "text.html",
                      "meta.directive.literal.javadoc",
                      "keyword.other.documentation.directive.literal.javadoc",
                      "punctuation.definition.keyword.javadoc")

    val sel1 = Selector.parse("keyword")
    assertEquals(4, sel1.matchDepth(scopes))
    val sel2 = Selector.parse("comment")
    assertEquals(0, sel2.matchDepth(scopes))
  }
}
