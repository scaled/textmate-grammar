//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import java.io.PrintStream

/** Provides a simple nested dictionaries format for grammars. */
object NDF {

  /** Simplifies the process of writing NDF. */
  class Writer (out :PrintStream, depth :Int) {
    private val prefix = " " * depth
    private def escape (text :String) = text.replace("\n", "\\\n")

    /** Emits the supplied key/value pair at the current depth. */
    def emit (key :String, value :String) {
      println(s"$key: ${escape(value)}")
    }

    /** Emits the supplied key/value pair iff `value` is defined. */
    def emit (key :String, value :Option[String]) {
      if (value.isDefined) emit(key, value.get)
    }

    /** Emits the key for a nested dictionary and returns a writer that can be used to write the
      * contents of said dictionary. */
    def nest (key :String) :Writer = {
      println(s"$key:")
      new Writer(out, depth+1)
    }

    private def println (text :String) {
      out.print(prefix)
      out.println(text)
    }
  }
}
