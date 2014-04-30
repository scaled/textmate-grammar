//
// Scaled - a scalable editor extensible via JVM languages
// http://github.com/samskivert/scaled/blob/master/LICENSE

package scaled.grammar

import java.io.PrintStream
import scala.collection.mutable.{Builder, Map => MMap}
import scaled._

/** Models a single TextMate grammar rule. Instead of having one giant intertwingled mess, we model
  * grammar rules with a variety of subclasses, each of which handles a particular common case.
  * This means that there are various "expressible" grammar rules which we cannot model, but those
  * are nonsensical and ideally do not show up in the wild.
  */
abstract class Rule {

  /** Compiles this rule into a sequence of matchers. */
  def compile (incFn :String => List[Matcher]) :List[Matcher]

  /** Prints a debug representation of this rule. */
  def print (out :PrintStream, depth :Int) :Unit

  /** Adds any scope names matched by this rule to `names`. */
  def collectNames (names :Builder[String,_]) {}

  protected def print (out :PrintStream, depth :Int, text :String) {
    out.print(" " * depth)
    out.println(text)
  }
}

object Rule {

  case class Include (group :String) extends Rule {
    override def compile (incFn :String => List[Matcher]) = List(new Matcher.Deferred(group, incFn))
    override def print (out :PrintStream, depth :Int) = print(out, depth, s"Include($group)")
  }

  case class Container (patterns :List[Rule]) extends Rule {
    override def compile (incFn :String => List[Matcher]) = patterns.flatMap(_.compile(incFn))
    override def print (out :PrintStream, depth :Int) = patterns.foreach(_.print(out, depth+1))
    override def collectNames (names :Builder[String,_]) = patterns.foreach(_.collectNames(names))
  }

  case class Single (pattern :String, name :Option[String], captures :List[(Int,String)])
      extends Rule {
    override def compile (incFn :String => List[Matcher]) = {
      val caps = name.map(n => (0 -> n) :: captures).getOrElse(captures)
      List(new Matcher.Single(Matcher.pattern(pattern, caps)))
    }
    override def print (out :PrintStream, depth :Int) =
      print(out, depth, s"Single($pattern, $name, ${fmt(captures)}")
    override def collectNames (names :Builder[String,_]) {
      name.foreach(names += _)
      names ++= captures.map(_._2)
    }
  }

  case class Multi (begin :String, beginCaptures :List[(Int,String)],
                    end :String, endCaptures :List[(Int,String)],
                    name :Option[String], contentName :Option[String],
                    patterns :List[Rule]) extends Rule {
    override def compile (incFn :String => List[Matcher]) = List(
      new Matcher.Multi(Matcher.pattern(begin, beginCaptures), Matcher.pattern(end, endCaptures),
                        name, contentName, patterns.flatMap(_.compile(incFn))))
    override def print (out :PrintStream, depth :Int) {
      print(out, depth, "Multi(" +
        s"begin='$begin' ${fmt(beginCaptures)}, end='$end' ${fmt(endCaptures)}, " +
        s"nm=${name getOrElse "<none>"}, cnm=${contentName getOrElse "<none>"})")
      patterns.foreach(_.print(out, depth+1))
    }
    override def collectNames (names :Builder[String,_]) {
      name.foreach(names += _)
      contentName.foreach(names += _)
      names ++= beginCaptures.map(_._2)
      names ++= endCaptures.map(_._2)
    }
  }

  private def fmt (caps :List[(Int,String)]) = caps.map(c => s"${c._1}:${c._2}").mkString(" ")
}
