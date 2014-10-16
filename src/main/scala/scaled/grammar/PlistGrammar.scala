//
// Scaled TextMate Grammar - a library for using TextMate language grammars with Scaled
// http://github.com/samskivert/scaled-textmate-grammar/blob/master/LICENSE

package scaled.grammar

import com.dd.plist._
import java.io.{File, InputStream}
import scaled._

private object PlistGrammar {

  def parse (file :File) = toGrammar(PropertyListParser.parse(file))
  def parse (in :InputStream) = toGrammar(PropertyListParser.parse(in))

  def toGrammar (root :NSObject) :Grammar = {
    val rootDict = root.asInstanceOf[NSDictionary]
    val name = stringFor(rootDict, "name") getOrElse "unknown:name"
    val scopeName = stringFor(rootDict, "scopeName") getOrElse "unknown:scopeName"
    val foldStart = stringFor(rootDict, "foldingStartMarker")
    val foldStop  = stringFor(rootDict, "foldingStopMarker")

    // val fileTypes = rootDict.objectForKey("fileTypes")
    new Grammar(name, scopeName, foldStart, foldStop) {
      val repository = Map((dictFor(rootDict, "repository").getHashMap.toMapV.flatMap {
        case (k, v) => parseRule(v).map(vr => (k -> vr))
      }))
      val patterns = parseRules(rootDict)
    }
  }

  def stringFor (dict :NSDictionary, key :String) = dict.objectForKey(key) match {
    case nsstr :NSString => Some(nsstr.toString)
    case _ => None
  }
  def dictFor (dict :NSDictionary, key :String) = dict.objectForKey(key) match {
    case dval :NSDictionary => dval
    case _ => new NSDictionary()
  }
  def arrayFor (dict :NSDictionary, key :String) = dict.objectForKey(key) match {
    case aval :NSArray => aval
    case _ => new NSArray()
  }

  def parseCaptures (dict :NSDictionary) = List() ++ dict.toMapV flatMap {
    case (group, vdict :NSDictionary) => Some(group.toInt -> stringFor(vdict, "name").get)
    case _ => None
  }

  def parseRules (dict :NSDictionary) :List[Rule] =
    List.from(arrayFor(dict, "patterns").getArray).flatMap(parseRule)

  def parseRule (data :NSObject) :Option[Rule] = try {
    val dict = data.asInstanceOf[NSDictionary]
    val include = stringFor(dict, "include")
    val name = stringFor(dict, "name")
    val `match` = stringFor(dict, "match")
    val begin = stringFor(dict, "begin")

    val rule = if (include.isDefined) {
      new Rule.Include(include.get)
    } else if (begin.isDefined) {
      val caps = parseCaptures(dictFor(dict, "captures"))
      val beginCaps = if (!dict.containsKey("beginCaptures")) caps else
        parseCaptures(dictFor(dict, "beginCaptures"))
      val end = stringFor(dict, "end") getOrElse {
        throw new Exception(s"Rule missing end: $name ${`match`}")
      }
      val endCaps = if (!dict.containsKey("endCaptures")) caps else
        parseCaptures(dictFor(dict, "endCaptures"))
      new Rule.Multi(begin.get, beginCaps, end, endCaps, name, stringFor(dict, "contentName"),
                     parseRules(dict))
    } else if (`match`.isDefined) {
      new Rule.Single(`match`.get, name, parseCaptures(dictFor(dict, "captures")))
    } else {
      new Rule.Container(parseRules(dict))
    }
    Some(rule)

  } catch {
    case e :Exception =>
      println(s"Rule parse failure: $data")
      e.printStackTrace(System.out)
      None
  }
}
