//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import java.io.File

object Convert {

  def main (args :Array[String]) {
    if (args.isEmpty) {
      println("Usage: scaled.grammar.Convert file.tmLanguage");
      sys.exit(1)
    }

    val grammar = PlistGrammar.parse(new File(args(0)))
    grammar.print(System.out)
  }
}
