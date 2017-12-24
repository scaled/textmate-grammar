//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import java.util.HashMap
import scaled._
import scaled.util.Errors

class GrammarManager (
  msvc :MetaService, psvc :PluginService
) extends AbstractService with GrammarService {

  private val plugins = new HashMap[String,GrammarPlugin]()
  private def pluginAdded (plugin :GrammarPlugin) = plugin.grammars.keySet foreach {
    scope => plugins.put(scope, plugin) }
  private def pluginRemoved (plugin :GrammarPlugin) = plugin.grammars.keySet foreach {
    scope => if (plugins.get(scope) == plugin) plugins.remove(scope) }

  private val plugset = psvc.resolvePlugins[GrammarPlugin]("textmate-grammar")
  plugset.plugins foreach pluginAdded
  plugset.added onValue pluginAdded
  plugset.removed onValue pluginRemoved

  private val comps = new HashMap[String,Grammar.Compiler]()
  private def compiler (scope :String) :Grammar.Compiler =
    Mutable.getOrPut(comps, scope, plugins.get(scope) match {
      case null => null
      case plugin => new Grammar.Compiler(plugin.grammar(scope), msvc.log, compiler)
    })

  override def didStartup () {}
  override def willShutdown () {}

  override def grammar (langScope :String) :Option[Grammar] =
    Option(compiler(langScope)).map(_.grammar)

  override def scoper (buffer :Buffer, langScope :String,
                       mkProcs :GrammarPlugin => List[Selector.Processor]) :Scoper = {
    val plugin = Option(plugins.get(langScope)) || {
      throw Errors.feedback(s"No grammar available for '$langScope'") }
    val comp = compiler(langScope)
    new Scoper(comp.grammar, Matcher.first(comp.matchers), buffer, mkProcs(plugin))
  }
}
