//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import java.io.InputStream
import java.nio.file.Path
import scaled._

object NDFGrammar {
  import NDF._

  def parse (file :Path) = toGrammar(read(file))
  def parse (in :InputStream) = toGrammar(read(in))

  def toGrammar (root :Seq[Entry]) :Grammar = {
    val (strs, dicts) = toMaps(root)
    val name = strs.get("name") || "unknown:name"
    val scopeName = strs.get("scopeName") || "unknown:scopeName"
    val foldStart = strs.get("foldStart")
    val foldStop  = strs.get("foldStop")

    // val fileTypes = rootDict.objectForKey("fileTypes")
    new Grammar(name, scopeName, foldStart, foldStop) {
      val repository = Map((dicts.get("repository") || Seq()).flatMap {
        case DicEnt(k, v) => Some(k -> new Rule.Container(v.flatMap(parseRule).toList))
        case e @ StrEnt(k, v) => println("Invalid repository entry: $e") ; None
      })
      val patterns = parseRules(dicts)
    }
  }

  def toMaps (es :Seq[Entry]) :(Map[String,String],Map[String,Seq[Entry]]) = {
    val (strs, dicts) = (Map.builder[String,String](), Map.builder[String,Seq[Entry]]())
    es foreach {
      case StrEnt(k, v)  => strs += (k -> v)
      case DicEnt(k, vs) => dicts += (k -> vs)
    }
    (strs.build(), dicts.build())
  }

  def parseCaptures (str :String) = {
    def parse (cap :String) = cap.indexOf("=") match {
      case -1 => println(s"Invalid capture: '$cap'") ; None
      case ii => Some(cap.substring(0, ii).toInt -> cap.substring(ii+1))
    }
    List.from(str.split("\\s+")).flatMap(parse)
  }
  def parseCaptures (str :Option[String]) :List[(Int,String)] = str.map(parseCaptures).getOrElse(List())

  def parseRules (dict :Map[String,Seq[Entry]]) :List[Rule] =
    (dict.get("patterns") || List()).flatMap(parseRule).toList

  def parseRule (data :Entry) :Option[Rule] = try data match {
    case StrEnt("include", target) => Some(new Rule.Include(target))
    case DicEnt("single", rdata) =>
      val (strs, dicts) = toMaps(rdata)
      Some(new Rule.Single(strs("pattern"), strs.get("name"), parseCaptures(strs.get("caps"))))
    case DicEnt("multi", rdata) =>
      val (strs, dicts) = toMaps(rdata)
      val begin = strs.get("begin") getOrElse {
        throw new Exception(s"Rule missing begin: $rdata")
      }
      val caps = parseCaptures(strs.get("caps"))
      val beginCaps = strs.get("bcaps").map(parseCaptures).getOrElse(caps)
      val end = strs.get("end") getOrElse {
        throw new Exception(s"Rule missing end: $rdata")
      }
      val endCaps = strs.get("ecaps").map(parseCaptures).getOrElse(caps)
      Some(new Rule.Multi(begin, beginCaps, end, endCaps, strs.get("name"),
                          strs.get("contentName"), parseRules(dicts)))
    case _ => println("Invalid rule data: $data") ; None

  } catch {
    case e :Exception =>
      println(s"Rule parse failure: $data")
      e.printStackTrace(System.out)
      None
  }
}
