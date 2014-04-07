//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import java.io.{File, StringReader}
import org.junit.Assert._
import org.junit._
import scaled.Loc
import scaled.impl.BufferImpl

class GrammarTest {

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
    /*18*/ "   public void test (int count) {}",
    /*19*/ "}").mkString("\n")

  def testBuffer (name :String, text :String) =
    BufferImpl(name, new File(name), new StringReader(text))

  @Test def testJavaDocMatch () {
    val buffer = testBuffer("Test.java", testJavaCode)
    val scoper = new Scoper(Seq(javaDoc), buffer)
    // println(scoper)
    assertEquals(List("comment.block.documentation.javadoc",
                      "punctuation.definition.comment.begin.javadoc"), scoper.scopesAt(Loc(2, 0)))
    assertEquals(List("comment.block.documentation.javadoc",
                      "meta.documentation.comment.javadoc",
                      "text.html",
                      "meta.directive.literal.javadoc",
                      "keyword.other.documentation.directive.literal.javadoc",
                      "punctuation.definition.keyword.javadoc"), scoper.scopesAt(Loc(3, 8)))
    assertEquals(List("comment.block.documentation.javadoc",
                      "meta.documentation.comment.javadoc",
                      "text.html",
                      "meta.directive.literal.javadoc",
                      "keyword.other.documentation.directive.literal.javadoc"),
                 scoper.scopesAt(Loc(3, 9)))
  }

  @Test def testParse () {
    val javaDoc = getClass.getClassLoader.getResourceAsStream("JavaDoc.tmLanguage")
    val java = getClass.getClassLoader.getResourceAsStream("Java.tmLanguage")
    val grammars = Seq(Grammar.parse(javaDoc), Grammar.parse(java))
    val buffer = testBuffer("Test.java", testJavaCode)
    val scoper = new Scoper(grammars, buffer)
    println(scoper)
  }
}
