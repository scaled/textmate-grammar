//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import org.junit.Assert._
import org.junit._
import scaled._

class SelectorTest {

  @Test def testMatchDepth () {
    val scopes = List("punctuation.definition.keyword.javadoc",
                      "keyword.other.documentation.directive.literal.javadoc",
                      "meta.directive.literal.javadoc",
                      "text.html",
                      "meta.documentation.comment.javadoc",
                      "comment.block.documentation.javadoc")

    val sel1 = Selector.parse("keyword")
    assertEquals(5, sel1.matchDepth(scopes))
    val sel2 = Selector.parse("comment")
    assertEquals(1, sel2.matchDepth(scopes))
  }
}
