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
    * @return the scoper or `None` if no grammar is available for `langScope`. */
  def scoper (buffer :Buffer, langScope :String) :Option[Scoper] =
    scoper(buffer, langScope, plugin => List(plugin.effacers, plugin.syntaxers).
      flatMap(sels => if (sels.isEmpty) None else Some(new Selector.Processor(sels))))

  /** Creates a [[Scoper]] for `buffer` using `langScope` to identify the main grammar.
    * @param mkProcs a function to create custom line processors given the plugin metadata.
    * @return the scoper or `None` if no grammar is available for `langScope`. */
  def scoper (buffer :Buffer, langScope :String,
              mkProcs :GrammarPlugin => List[Selector.Processor]) :Option[Scoper]
}
