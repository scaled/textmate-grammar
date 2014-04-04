//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import scala.collection.mutable.{Map => MMap}
import scaled._

/** Models a single TextMate grammar rule. Instead of having one giant intertwingled mess, we model
  * grammar rules with a variety of subclasses, each of which handles a particular common case.
  * This means that there are various "expressible" grammar rules which we cannot model, but those
  * are nonsensical and ideally do not show up in the wild.
  */
abstract class Rule {

  /** Compiles this rule into a sequence of matchers. */
  def compile (langFn :String => List[Matcher], repo :Map[String,Rule],
               cache :MMap[String,List[Matcher]]) :List[Matcher]
}

object Rule {

  case class Include (group :String) extends Rule {
    override def compile (langFn :String => List[Matcher], repo :Map[String,Rule],
                          cache :MMap[String,List[Matcher]]) =
      // TODO: this is going to infinite loop on recursive patterns
      if (group startsWith "#") cache.getOrElseUpdate(
        group substring 1, repo(group substring 1).compile(langFn, repo, cache))
      else if (group == "$self") Nil // TODO
      else langFn(group) // otherwise this is a reference to a different language
  }

  case class Container (patterns :List[Rule]) extends Rule {
    override def compile (langFn :String => List[Matcher], repo :Map[String,Rule],
                          cache :MMap[String,List[Matcher]]) =
      patterns.flatMap(_.compile(langFn, repo, cache))
  }

  case class Single (pattern :String, name :Option[String], captures :List[(Int,String)])
      extends Rule {
    override def compile (langFn :String => List[Matcher], repo :Map[String,Rule],
                          cache :MMap[String,List[Matcher]]) = {
      val caps = name.map(n => (0 -> n) :: captures).getOrElse(captures)
      List(new Matcher.Single(new Matcher.Pattern(pattern, caps)))
    }
  }

  case class Multi (begin :String, beginCaptures :List[(Int,String)],
                    end :String, endCaptures :List[(Int,String)],
                    name :Option[String], contentName :Option[String],
                    patterns :List[Rule]) extends Rule {
    override def compile (langFn :String => List[Matcher], repo :Map[String,Rule],
                          cache :MMap[String,List[Matcher]]) = List(
      new Matcher.Multi(new Matcher.Pattern(begin, beginCaptures),
                        new Matcher.Pattern(end, endCaptures),
                        name, contentName, patterns.flatMap(_.compile(langFn, repo, cache))))
  }
}
