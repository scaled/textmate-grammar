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
  def compile (incFn :String => List[Matcher]) :List[Matcher]
}

object Rule {

  case class Include (group :String) extends Rule {
    override def compile (incFn :String => List[Matcher]) = List(new Matcher.Deferred(group, incFn))
  }

  case class Container (patterns :List[Rule]) extends Rule {
    override def compile (incFn :String => List[Matcher]) = patterns.flatMap(_.compile(incFn))
  }

  case class Single (pattern :String, name :Option[String], captures :List[(Int,String)])
      extends Rule {
    override def compile (incFn :String => List[Matcher]) = {
      val caps = name.map(n => (0 -> n) :: captures).getOrElse(captures)
      List(new Matcher.Single(Matcher.pattern(pattern, caps)))
    }
  }

  case class Multi (begin :String, beginCaptures :List[(Int,String)],
                    end :String, endCaptures :List[(Int,String)],
                    name :Option[String], contentName :Option[String],
                    patterns :List[Rule]) extends Rule {
    override def compile (incFn :String => List[Matcher]) = List(
      new Matcher.Multi(Matcher.pattern(begin, beginCaptures), Matcher.pattern(end, endCaptures),
                        name, contentName, patterns.flatMap(_.compile(incFn))))
  }
}
