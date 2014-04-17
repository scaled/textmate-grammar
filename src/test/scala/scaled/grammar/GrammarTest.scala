//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import java.io.{File, StringReader}
import org.junit.Assert._
import org.junit._
import scala.collection.mutable.ArrayBuffer
import scaled._
import scaled.impl.BufferImpl
import scaled.major.SyntaxTable

class GrammarTest {

  val syntax = new SyntaxTable()

  val JdBegin = "punctuation.definition.directive.begin.javadoc"
  val JdEnd   = "punctuation.definition.directive.end.javadoc"
  val JdKey   = "punctuation.definition.keyword.javadoc"

  val KeyDoc  = (kind :String) => s"keyword.other.documentation.$kind.javadoc"
  val KeyDir  = (kind :String) => s"keyword.other.documentation.directive.$kind.javadoc"

  val javaDoc = new Grammar(
    name      = "JavaDoc",
    scopeName = "text.html.javadoc",
    foldingStartMarker = Some("""/\*\*"""),
    foldingStopMarker  = Some("""\*\*/""")) {

    val repository = Map(
      "inline" -> rules(
        include("#invalid"),
        include("#inline-formatting"),
        // include("text.html.basic"),
        single("""((https?|s?ftp|ftps|file|smb|afp|nfs|(x-)?man|gopher|txmt):""" +
               """\/\/|mailto:)[-:@a-zA-Z0-9_.~%+\/?=&#]+(?<![.?:])""",
               name = Some("markup.underline.link"))),

      "inline-formatting" -> rules(
        map("code", "literal") { kind =>
          multi(
            begin = s"(\\{)((\\@)$kind)",
            beginCaptures = List(1 -> JdBegin, 2 -> KeyDir(kind), 3 -> JdKey),
            end = """\}""",
            endCaptures = List(0 -> JdEnd),
            name = Some(s"meta.directive.$kind.javadoc"),
            contentName = Some(s"markup.raw.$kind.javadoc"))
        } ++ map("docRoot", "inheritDoc") { kind =>
          single("""(\{)((\@)$kind)(\})""",
                 name = Some(s"meta.directive.$kind.javadoc"),
                 captures = List(1 -> JdBegin, 2 -> KeyDir(kind), 3 -> JdKey, 4 -> JdEnd))
        } ++ map("link", "linkplain") { kind =>
          single(s"(\\{)((\\@)$kind)(?:\\s+(\\S+?))?(?:\\s+(.+?))?\\s*(\\})",
                 name = Some(s"meta.directive.$kind.javadoc"),
                 captures = List(1 -> JdBegin, 2 -> KeyDir(kind), 3 -> JdKey,
                                 4 -> s"markup.underline.$kind.javadoc",
                                 5 -> "string.other.link.title.javadoc",
                                 6 -> JdEnd))
        } ++ Seq(
          single("""(\{)((\@)value)\s*(\S+?)?\s*(\})""",
                 name = Some("meta.directive.value.javadoc"),
                 captures = List(1 -> JdBegin, 2 -> KeyDir("value"), 3 -> JdKey,
                                 4 -> "variable.other.javadoc", 5 -> JdEnd))) :_*),

      "invalid" -> rules(
        single("""^(?!\s*\*).*$\n?""", Some("invalid.illegal.missing-asterisk.javadoc")))
    )

    // we have to specify a return type here to work around scalac bug; meh
    val patterns :List[Rule] = List(
      multi(
        begin = """(/\*\*)\s*""",
        beginCaptures = List(1 -> "punctuation.definition.comment.begin.javadoc"),
        end = """\*/""",
        endCaptures = List(0 -> "punctuation.definition.comment.end.javadoc"),
        name = Some("comment.block.documentation.javadoc"),
        patterns = List(
          include("#invalid"),
          multi(
            begin = """\*\s*(?=\w)""",
            end = """(?=\s*\*\s*@)|(?=\s*\*\s*/)""",
            name = Some("meta.documentation.comment.javadoc"),
            contentName = Some("text.html"),
            patterns = List(include("#inline")))) ++
          map("param", "return", "throws", "exception", "author", "version", "see", "since",
              "serial", "serialField", "serialData", "deprecated") { kind =>
            multi(
              begin = s"\\*\\s*((\\@)$kind)",
              beginCaptures = List(1 -> KeyDoc(kind), 2 ->  JdKey),
              end = """(?=\s*\*\s*@)|(?=\s*\*\s*/)""",
              name = Some(s"meta.documentation.tag.$kind.javadoc"),
              contentName = Some("text.html"),
              patterns = List(include("#inline")))
          } ++ List(
            single("""\*\s*((\@)\S+)\s""", captures = List(1 -> KeyDoc("custom"), 2 -> JdKey)))))
  }

  val testJavaCode = Seq(
    //                1         2         3         4         5         6         7         8
    //      012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
    /* 0*/ "package foo;",
    /* 1*/ "",
    /* 2*/ "/** This is some test Java code that we'll use to test {@code Grammar} and specifically",
    /* 3*/ " * the {@literal JavaDoc} grammar.",
    /* 4*/ " * @see http://manual.macromates.com/en/language_grammars",
    /* 5*/ " */",
    /* 6*/ "public class Test {",
    /* 7*/ "   /**",
    /* 8*/ "    * A constructor, woo!",
    /* 8*/ "    * @param foo for fooing.",
    /*10*/ "    * @param bar for barring.",
    /*11*/ "    */",
    /*12*/ "   public Test () {}",
    /*13*/ "",
    /*14*/ "   /**",
    /*15*/ "    * A method. How exciting. Let's {@link Test} to something.",
    /*16*/ "    * @throws IllegalArgumentException if we feel like it.",
    /*17*/ "    */",
    /*18*/ "   @Deprecated(\"Use peanuts\")",
    /*19*/ "   public void test (int count) {}",
    /*20*/ "}").mkString("\n")

  val commentStart = List("text.html.javadoc",
                          "comment.block.documentation.javadoc",
                          "punctuation.definition.comment.begin.javadoc")
  val literalAt = List("text.html.javadoc",
                       "comment.block.documentation.javadoc",
                       "meta.documentation.comment.javadoc",
                       "text.html",
                       "meta.directive.literal.javadoc",
                       "keyword.other.documentation.directive.literal.javadoc",
                       "punctuation.definition.keyword.javadoc")
  val literalToken = List("text.html.javadoc",
                          "comment.block.documentation.javadoc",
                          "meta.documentation.comment.javadoc",
                          "text.html",
                          "meta.directive.literal.javadoc",
                          "keyword.other.documentation.directive.literal.javadoc")

  def testBuffer (name :String, text :String) =
    BufferImpl(name, new File(name), new StringReader(text))

  @Test def testParse () {
    val javaDoc = getClass.getClassLoader.getResourceAsStream("JavaDoc.tmLanguage")
    val java = getClass.getClassLoader.getResourceAsStream("Java.tmLanguage")
    val grammars = Seq(Grammar.parse(javaDoc), Grammar.parse(java))
    val buffer = testBuffer("Test.java", testJavaCode)
    val scoper = new Scoper(grammars, buffer, syntax, Nil)
    // println(scoper)
  }

  @Test def testJavaDocMatch () {
    val buffer = testBuffer("Test.java", testJavaCode)
    val scoper = new Scoper(Seq(javaDoc), buffer, syntax, Nil)
    // println(scoper.showScopes)
    assertEquals(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertEquals(literalAt, scoper.scopesAt(Loc(3, 8)))
    assertEquals(literalToken, scoper.scopesAt(Loc(3, 9)))
  }

  val smallTestCode = Seq(
    //                1         2         3         4         5         6
    //      012345678901234567890123456789012345678901234567890123456789012345678
    /* 0*/ "package foo;",
    /* 1*/ "",
    /* 2*/ "/**", // 901234567890123456789012345678901234567890123456789012345678
    /* 3*/ " * Blah blah {@literal Grammar} blah blah {@code JavaDoc} grammar.",
    /* 4*/ " * @see http://manual.macromates.com/en/language_grammars",
    /* 5*/ " */").mkString("\n")

  // grinds through buffer generating a 2D array of the scopes at every position
  def mapScopes (buffer :Buffer, scoper :Scoper) :Seq[Seq[List[String]]] = {
    val map = ArrayBuffer[ArrayBuffer[List[String]]]()
    var pos = buffer.start ; val end = buffer.end
    while (pos < end) {
      if (map.length <= pos.row) map += ArrayBuffer[List[String]]()
      map(pos.row) += scoper.scopesAt(pos)
      pos = buffer.forward(pos, 1)
    }
    map
  }

  def smallTestBits (showScopes :Boolean) = {
    val buffer = testBuffer("Test.java", smallTestCode)
    val scoper = new Scoper(Seq(javaDoc), buffer, syntax, Nil)
    if (showScopes) println(scoper.showScopes)
    // do some precondition tests
    assertEquals(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertEquals(literalAt, scoper.scopesAt(Loc(3, 14)))
    assertEquals(literalToken, scoper.scopesAt(Loc(3, 15)))
    (buffer, scoper)
  }

  @Test def testWordInsert () {
    val (buffer, scoper) = smallTestBits(false)
    buffer.insert(Loc(3, 8), "blah ", Styles.None)
    // println(scoper.showScopes)
    assertEquals(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertEquals(literalAt, scoper.scopesAt(Loc(3, 19)))
    assertEquals(literalToken, scoper.scopesAt(Loc(3, 20)))
  }

  @Test def testNewlineInsert () {
    val (buffer, scoper) = smallTestBits(false)
    buffer.insert(Loc(3, 0), Seq(new Line(""), new Line("")))
    // println(scoper.showScopes)
    assertEquals(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertEquals(literalAt, scoper.scopesAt(Loc(4, 14)))
    assertEquals(literalToken, scoper.scopesAt(Loc(4, 15)))
  }

  @Test def testRaggedInsert () {
    val (buffer, scoper) = smallTestBits(false)
    buffer.insert(Loc(3, 0), Seq(new Line(" "), new Line(" ")))
    // println(scoper.showScopes)
    assertEquals(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertEquals(literalAt, scoper.scopesAt(Loc(4, 15)))
    assertEquals(literalToken, scoper.scopesAt(Loc(4, 16)))
  }

  @Test def testWordDelete () {
    val (buffer, scoper) = smallTestBits(false)
    buffer.delete(Loc(3, 8), 5)
    // println(scoper.showScopes)
    assertEquals(commentStart, scoper.scopesAt(Loc(2, 0)))
    assertEquals(literalAt, scoper.scopesAt(Loc(3, 9)))
    assertEquals(literalToken, scoper.scopesAt(Loc(3, 10)))
  }

  @Test def testNewlineDelete () {
    val (buffer, scoper) = smallTestBits(false)
    buffer.delete(Loc(1, 0), Loc(2, 0))
    // println(scoper.showScopes)
    assertEquals(commentStart, scoper.scopesAt(Loc(1, 0)))
    assertEquals(literalAt, scoper.scopesAt(Loc(2, 14)))
    assertEquals(literalToken, scoper.scopesAt(Loc(2, 15)))
  }

  @Test def testEnclosingDelete () {
    val (buffer, scoper) = smallTestBits(true)
    buffer.delete(Loc(3, 13), Loc(3, 32))
    println(scoper.showScopes)
    assertEquals(commentStart, scoper.scopesAt(Loc(2, 0)))
    val scopes = List("text.html.javadoc",
                      "comment.block.documentation.javadoc",
                      "meta.documentation.comment.javadoc",
                      "text.html")
    assertEquals(scopes, scoper.scopesAt(Loc(3, 14)))
    assertEquals(scopes, scoper.scopesAt(Loc(3, 15)))
  }

  // TODO: other deletion tests, but things seem to work properly now and I'm too lazy
}
