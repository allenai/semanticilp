package org.allenai.ari.solvers.textilp.utils

import java.net.URLEncoder

import org.allenai.ari.solvers.textilp.{Paragraph, Question}
import org.allenai.common.cache.JsonQueryCache

import scala.io.Source
import play.api.libs.json._

object WikiUtils {
  def extractRelevantCategories(category: String): Seq[String] = {
    val wikiCategoryQueryUrl = s"https://en.wikipedia.org/w/api.php?action=query&list=categorymembers&cmtitle=Category:${URLEncoder.encode(category, "UTF-8")}&cmlimit=100&format=json"
    val html = Source.fromURL(wikiCategoryQueryUrl)
    val jsonString = html.mkString
    val json = Json.parse(jsonString)
    (json \\ "title").map { jsValue =>
      jsValue.as[JsString]
    }.collect{case jsValue if !jsValue.value.contains("List of") && !jsValue.value.contains("Template:") => jsValue.value}
  }

  // the Bishop
  def extractCategoryOfWikipage(wikipageMention: String): Seq[String] = {
    val wikiCategoryQueryUrl = s"https://en.wikipedia.org/w/api.php?format=json&action=query&prop=categories&titles=${URLEncoder.encode(wikipageMention, "UTF-8")}"
    val html = Source.fromURL(wikiCategoryQueryUrl)
    val jsonString = html.mkString
    // println(jsonString)
    val json = Json.parse(jsonString)
    (json \\ "title").map { jsValue =>
      jsValue.as[JsString]
    }.collect{case jsValue if jsValue.value.contains("Category:") => jsValue.value}
  }

  def extractCategoryOfWikipageRecursively(wikipageMentions: Seq[String], depth: Int): Seq[String] = {
    if(depth > 0) {
      val mentionsDepthLower = wikipageMentions.flatMap{ m => extractCategoryOfWikipage(m) }
      extractCategoryOfWikipageRecursively(mentionsDepthLower.toSet.union(wikipageMentions.toSet).toSeq, depth - 1)
    }
    else {
      wikipageMentions
    }
  }

  // this function gets the Wikipedia id of the page and returns its WikiData id
  def getWikiDataId(title: String): Option[String] = {
    val wikiCategoryQueryUrl = s"https://en.wikipedia.org/w/api.php?action=query&prop=pageprops&format=json&titles=${URLEncoder.encode(title, "UTF-8")}"
    val html = Source.fromURL(wikiCategoryQueryUrl)
    val jsonString = html.mkString
    val json = Json.parse(jsonString)
    (json \\ "wikibase_item").map { jsValue =>
      jsValue.as[JsString]
    }.map{ _.value }.headOption
  }

  // important WikiData relations
  object WikiDataProperties {
    // property
    val instanceOf = "P31"
    val subclassOf = "P279"

    // entity
    val person = "Q5"
    val country = "Q6256"
    val sovereignState = "Q3624078"
    val memberOfUN = "Q160016"
    val city = "Q515"
    val mountain = "Q8502"
    val state = "Q7275"
    val drink = "Q40050"
    val food = "Q2095"
    val color = "Q1075"
  }

//  def wikiAskQueryWithMaxRepetition(qStr: String, pStr: String, property: String, maxRepetition: Int): Boolean = {
//    val result = (1 to maxRepetition).exists(wikiAskQuery(qStr, pStr, property, _))
//    println(s"$qStr -> $property -> $pStr: $result")
//    result
//  }

  import redis.clients.jedis.Protocol
  import spray.json.DefaultJsonProtocol._
  lazy val wikiDataRedis = JsonQueryCache[String]("", "localhost", Protocol.DEFAULT_PORT, Protocol.DEFAULT_TIMEOUT)

  def wikiAskQuery(qStr: String, pStr: String, property: String, mostOccurrences: Int): Boolean = {
    /*
    val qStrId = getWikiDataId(qStr)
    val pStrId = getWikiDataId(pStr)
    //println(s"qStr: $qStr /  qStrId: $qStrId / pStr: $pStr / pStrId: $pStrId")
    val result = if(qStrId.isDefined && pStrId.isDefined) {
      val qStrIdEncoded = URLEncoder.encode(qStrId.get, "UTF-8")
      val pStrIdEncoded = URLEncoder.encode(pStrId.get, "UTF-8")
      val propertyEncoded = URLEncoder.encode(property, "UTF-8")
      require(mostOccurrences > 0)
      // no "?" for the first one
      val path = "wdt:" + propertyEncoded + "/" + (1 until mostOccurrences).map(_ => "wdt:" + propertyEncoded + "?").mkString("/")
      val wikiCategoryQueryUrl = s"https://query.wikidata.org/sparql?format=json&query=ASK%20{%20wd:$qStrIdEncoded%20$path%20wd:$pStrIdEncoded%20}"
      val redisResult = wikiDataRedis.get(wikiCategoryQueryUrl)
      if(redisResult.isEmpty) {
        println("qStr: " + qStr + " / pStr: " + pStr + " / query: " + wikiCategoryQueryUrl)
        val html = Source.fromURL(wikiCategoryQueryUrl)
        val jsonString = html.mkString
        val json = Json.parse(jsonString)
        val result = (json \ "boolean").as[JsBoolean].value
        wikiDataRedis.put(wikiCategoryQueryUrl, result.toString)
        result
      }
      else{
        println("qStr: " + qStr + " / pStr: " + pStr + " / from cache: " + redisResult.get)
        redisResult.get.toBoolean
      }
    }
    else {
      false
    }
//    println(s"$qStr -> $property -> $pStr: $result")
    result
    */
    false
  }

  def wikiDataDistanceWithIds(e1: String, e2: String, property: String): Int = {
    val endpoint = "https://query.wikidata.org/sparql?format=json&query="
    val url = s"""PREFIX gas: <http://www.bigdata.com/rdf/gas#>

                SELECT ?super (?aLength + ?bLength as ?length) WHERE {
                  SERVICE gas:service {
                    gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.SSSP" ;
                                gas:in wd:$e1 ;
                                gas:traversalDirection "Forward" ;
                                gas:out ?super ;
                                gas:out1 ?aLength ;
                                gas:maxIterations 10 ;
                                gas:linkType wdt:$property .
                  }
                  SERVICE gas:service {
                    gas:program gas:gasClass "com.bigdata.rdf.graph.analytics.SSSP" ;
                                gas:in wd:$e2 ;
                                gas:traversalDirection "Forward" ;
                                gas:out ?super ;
                                gas:out1 ?bLength ;
                                gas:maxIterations 10 ;
                                gas:linkType wdt:$property .
                  }
                } ORDER BY ?length
                LIMIT 1"""
    val query = endpoint + URLEncoder.encode(url, "UTF-8")
    val html = Source.fromURL(query)
    val jsonString = html.mkString
    val json = Json.parse(jsonString)
    (json \\ "length").map { jsValue =>
      (jsValue \ "value").get.as[JsString].value.toDouble.toInt
    }.headOption.getOrElse(30)
  }

  def wikiDistanceWithIds(e1: String, e2: String): Int = {
    math.min(
      wikiDataDistanceWithIds(e1, e2, WikiDataProperties.instanceOf),
      wikiDataDistanceWithIds(e1, e2, WikiDataProperties.subclassOf)
    )
  }

  def wikiDistance(e1: String, e2: String): Int = {
    val eId1 = getWikiDataId(e1)
    val eId2 = getWikiDataId(e2)
    //println(eId1 + "  /  " + eId2)
    if(eId1.isDefined && eId2.isDefined) {
      wikiDistanceWithIds(eId1.get, eId2.get)
    }
    else {
      20
    }
  }

  def getWikiBaseCandidatesForQuestion(question: Question): Seq[String] = {
    Seq.empty
  }
}
