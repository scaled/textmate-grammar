//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import scaled._

@Service(name="textmate-grammar", impl="GrammarManager",
         desc="Provides a database of TextMate grammars for syntax highlighting.")
trait GrammarService {

  /** Returns the grammar for `langScope`, if one is available. */
  def grammar (langScope :String) :Option[Grammar]

  /** Creates a [[Scoper]] for `buffer` using `langScope` to identify the main grammar.
    * @param mkProcs a function that creates the line processors given the plugin metadata. */
  def scoper (buffer :RBuffer, langScope :String,
              mkProcs :GrammarPlugin => List[Selector.Processor]) :Scoper
}
