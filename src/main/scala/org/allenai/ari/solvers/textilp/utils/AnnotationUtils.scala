package org.allenai.ari.solvers.textilp.utils

import java.util.Properties

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{Constituent, TextAnnotation}
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.core.utilities.configuration.{Configurator, ResourceManager}
import edu.illinois.cs.cogcomp.curator.{CuratorConfigurator, CuratorFactory}
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator._
import edu.illinois.cs.cogcomp.pipeline.main.PipelineFactory
//import edu.illinois.cs.cogcomp.quant.driver.Quantifier
import org.allenai.ari.solvers.textilp.utils.WikiUtils.WikiDataProperties
import org.allenai.ari.solvers.textilp.{Paragraph, Question, TopicGroup}
import org.allenai.common.cache.JsonQueryCache
import redis.clients.jedis.{JedisPool, Protocol}
import spray.json.DefaultJsonProtocol._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/** a dummy redis client for when redis is not supposed to be used */
object DummyRedisClient extends JsonQueryCache[String]("", new JedisPool()) {
  override def get(key: String) = None
  override def put(key: String, value: String): Unit = {}
}

sealed trait AnnotationLevel {}

// just tokenization
case object Tokenization extends AnnotationLevel
// Tokens, NER, POS, Chunk
case object Basic extends AnnotationLevel
// ???
case object Intermediate extends AnnotationLevel
// All possible annotations
case object Advanced extends AnnotationLevel

class AnnotationUtils {

  // redis cache for annotations
  lazy val synchronizedRedisClient = if (Constants.useRedisCachingForAnnotation) {
    JsonQueryCache[String]("", "localhost", Protocol.DEFAULT_PORT, Protocol.DEFAULT_TIMEOUT)
  } else {
    // use the dummy client, which always returns None for any query (and not using any Redis)
    DummyRedisClient
  }

  val viewsToDisable = Set(USE_SRL_NOM, USE_SRL_VERB, USE_STANFORD_DEP, USE_STANFORD_PARSE)

  lazy val pipelineService = {
    println("Starting to build the pipeline service . . . ")
    val settings = new Properties()
    settings.setProperty("cacheDirectory", "annotation-cache-textilp")
    //settings.setProperty("disableCache", Configurator.TRUE)
    viewsToDisable.foreach{ key => settings.setProperty(key.value, Configurator.FALSE) }
    val rm = new ResourceManager(settings)
    //viewsToDisable.foreach { v => settings.setProperty(v.key, Configurator.FALSE) }
    val config = new PipelineConfigurator().getConfig(rm)
    val service = PipelineFactory.buildPipeline(config)
    println("Done with building the pipeline service . . . ")
    service
  }

  lazy val curatorService = {
    //val settings = new Properties()
    //settings.setProperty("cacheDirectory", "annotation-cache-textilp")
    //settings.setProperty("disableCache", Configurator.TRUE)
    //viewsToDisable.foreach{ key => settings.setProperty(key.value, Configurator.FALSE) }
    //val rm = new ResourceManager(settings)
    //viewsToDisable.foreach { v => settings.setProperty(v.key, Configurator.FALSE) }
    //val config = new CuratorConfigurator().getConfig(rm)
    CuratorFactory.buildCuratorClient()
  }

  def annotate(string: String): TextAnnotation = {
    val cacheKey = "TextAnnotations:" + viewsToDisable.mkString("*") + string
    val redisAnnotation = synchronizedRedisClient.get(cacheKey)
    if (redisAnnotation.isDefined) {
      SerializationHelper.deserializeFromJson(redisAnnotation.get)
    } else {
      //val textAnnotation = pipelineService.createAnnotatedTextAnnotation("", "", string)
      val textAnnotation = pipelineService.createBasicTextAnnotation("", "", string)
      pipelineService.addView(textAnnotation, ViewNames.POS)
      pipelineService.addView(textAnnotation, ViewNames.NER_CONLL)
      pipelineService.addView(textAnnotation, ViewNames.NER_ONTONOTES)
      pipelineService.addView(textAnnotation, ViewNames.SHALLOW_PARSE)
      println("string" + string)
      pipelineService.addView(textAnnotation, ViewNames.QUANTITIES)
      //if (withQuantifier) quantifierAnnotator.addView(textAnnotation)
      synchronizedRedisClient.put(cacheKey, SerializationHelper.serializeToJson(textAnnotation))
      textAnnotation
    }
  }

  //lazy val quantifierAnnotator = new Quantifier()

  def annotateInstance(tg: TopicGroup): TopicGroup = {
    val annotatedParagraphs = tg.paragraphs.map { p =>
      val annotatedContext = pipelineService.createBasicTextAnnotation("", "", p.context)
      val annotatedQuestions = p.questions.map { q =>
        val ta = pipelineService.createBasicTextAnnotation("", "", q.questionText)
        Question(q.questionText, q.questionId, q.answers, Some(ta))
      }
      Paragraph(p.context, annotatedQuestions, Some(annotatedContext))
    }
    TopicGroup(tg.title, annotatedParagraphs)
  }

  // wikifier annotations for question candidate generation


//  val wikifierAnnotations = {
//    val lines = Source.fromFile("other/squadDev_wikifier.txt").getLines().mkString
//
//    lines.split("\n\n").map{ jsonString =>
//      println(jsonString)
//      val ta = SerializationHelper.deserializeFromJson(jsonString)
//      println(ta.getAvailableViews)
//    }
//    1.0
//  }

  import redis.clients.jedis.Protocol
  import spray.json.DefaultJsonProtocol._
  lazy val wikifierRedis = JsonQueryCache[String]("", "localhost", Protocol.DEFAULT_PORT, Protocol.DEFAULT_TIMEOUT)

  def processSQuADWithWikifierAndPutRedis(reader: SQuADReader) = {
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          val ta = curatorService.createBasicTextAnnotation("", "", p.context)
          curatorService.addView(ta, ViewNames.WIKIFIER)
          val json = SerializationHelper.serializeToJson(ta)
          wikifierRedis.put(ta.getText, json)
          p.questions.foreach { q =>
            val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
            curatorService.addView(ta, ViewNames.WIKIFIER)
            val json = SerializationHelper.serializeToJson(ta)
            wikifierRedis.put(ta.getText, json)
          }
        }
    }
  }

  def processSQuADWithWikifier(reader: SQuADReader) = {
    import java.io._
    val pw = new PrintWriter(new File("squadTrain_wikifier.txt" ))
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          val ta = curatorService.createBasicTextAnnotation("", "", p.context)
          curatorService.addView(ta, ViewNames.WIKIFIER)
          val json = SerializationHelper.serializeToJson(ta)
          pw.write(json + "\n\n")
          p.questions.foreach { q =>
            val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
            curatorService.addView(ta, ViewNames.WIKIFIER)
            val json = SerializationHelper.serializeToJson(ta)
            pw.write(json + "\n\n")
          }
        }
    }
    pw.close()
  }

  def verifyWikifierAnnotationsOnDisk(reader: SQuADReader) = {
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          //val ta = curatorService.createBasicTextAnnotation("", "", p.context)
          //curatorService.addView(ta, ViewNames.WIKIFIER)
          //val json = SerializationHelper.serializeToJson(ta)
          //wikifierRedis.put(p.context, json)
          val redisOutput = wikifierRedis.get(p.context)
          require(redisOutput.isDefined)
          p.questions.foreach { q =>
            //val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
            //curatorService.addView(ta, ViewNames.WIKIFIER)
            //val json = SerializationHelper.serializeToJson(ta)
            //wikifierRedis.put(ta.getText, json)
            val redisOutput = wikifierRedis.get(q.questionText)
            require(redisOutput.isDefined)
          }
        }
    }
  }

  import scala.collection.JavaConverters._

  val questionTerms = Set("which", "what", "where", "who", "whom", "how")
  val numberTriggers = Set("age")

  def containsOffset(c: Constituent, offset: Int): Boolean = {
    c.getEndCharOffset >= offset && c.getStartCharOffset <= offset
  }

  def getTargetPhrase(question: Question, paragraph: Paragraph) = {
    require(question.qTAOpt.isDefined, throw new Exception("the annotation for this question doest not exist"))
    require(paragraph.contextTAOpt.isDefined, throw new Exception("the annotation for this paragraph doest not exist"))
    println(question.questionText)
    println(paragraph.context)
    val paragraphTokenView = paragraph.contextTAOpt.get.getView(ViewNames.TOKENS)
    val paragraphTokens = paragraphTokenView.getConstituents.asScala.toList
    val shallowParseCons = question.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.toList
    val paragraphQuantitiesCons = paragraph.contextTAOpt.get.getView(ViewNames.QUANTITIES).getConstituents.asScala.toList
    val paragraphNerConsConll = paragraph.contextTAOpt.get.getView(ViewNames.NER_CONLL).getConstituents.asScala.toList
    val paragraphNerConsOnto = paragraph.contextTAOpt.get.getView(ViewNames.NER_ONTONOTES).getConstituents.asScala.toList
    val questionWikiAnnotationOpt = wikifierRedis.get(question.questionText)
    val paragraphWikiAnnotationOpt = wikifierRedis.get(paragraph.context)
    require(questionWikiAnnotationOpt.isDefined, throw new Exception("The wiki annotation for question is not defined"))
    require(paragraphWikiAnnotationOpt.isDefined, throw new Exception("The wiki annotation for paragraph is not defined"))
    val wikiMentionsInText = SerializationHelper.deserializeFromJson(paragraphWikiAnnotationOpt.get).getView(ViewNames.WIKIFIER).getConstituents.asScala.toList
    val wikiMentionsInQuestion = SerializationHelper.deserializeFromJson(questionWikiAnnotationOpt.get).getView(ViewNames.WIKIFIER).getConstituents.asScala.toList

    def questionPhraseCondition(a: Constituent): Boolean = {
      val splitSet = a.getSurfaceForm.toLowerCase.split(" ").toSet
      val fullText = a.getTextAnnotation.text.toLowerCase

      if(splitSet.contains("who")) {
        // who should not come with "those who" or "how many"
        if (fullText.contains("how many") || fullText.contains("those who")) {
          false
        }
        else {
          true
        }
      } else {
        splitSet.intersect(questionTerms).nonEmpty
      }
    }

    val questionConstituentOpt = shallowParseCons.zipWithIndex.collectFirst{ case (a, idx) if questionPhraseCondition(a) => a -> idx }

    println(shallowParseCons.map(a =>
      s"( ${a.getSurfaceForm} -> ${a.getLabel} )").mkString(" ")
    )
    println(questionConstituentOpt)
    val candidates = ArrayBuffer[Constituent]()
    if(questionConstituentOpt.isDefined) {
      val tailingTerms = questionConstituentOpt.get._1.getSurfaceForm.split(" ").tail
      val idx = questionConstituentOpt.get._2
      var triggerTerm = if(tailingTerms.nonEmpty) {
        tailingTerms.mkString(" ")
      }
      else {
        println("trigger terms was empty so we replaced it with the next constituents")
        shallowParseCons(idx + 1).getSurfaceForm
      }

      println("trigger term:  " + triggerTerm)

      // number trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.contains(" age ") ||
        questionConstituentOpt.get._1.getSurfaceForm.contains("how many")
      ){
        println("number trigger! ")
        candidates.++=:(paragraphQuantitiesCons.filter(_.getLabel.contains("Number")))
      }

      // date trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.contains(" date ")){
        println("date trigger! ")
        candidates.++=:(paragraphQuantitiesCons.filter(_.getLabel.contains("Date")))

        //
      }

      // person trigger
      if(( questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("who") &&
        !questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("how many") ) ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("which individual") ||
        questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("which person") ){
        println("person trigger! ")
        // NER-Conll
        candidates.++=:(paragraphNerConsConll.filter(_.getLabel.contains("PER")))
        println("surface: " + paragraphNerConsConll)
        println("labels: " + paragraphNerConsConll.map(_.getLabel))
        println("conll filtered: " + paragraphNerConsConll.filter(_.getLabel.contains("PER")))

        // NER-Ontonotes
        val ontonotesCandidates = paragraphNerConsOnto.filter(_.getLabel.contains("PERSON"))
        candidates.++=:(ontonotesCandidates)
        println(paragraphNerConsOnto)

        // add constituents with prefix
        val prefixSet = Seq("Father", "Rev.")
        val constituentsWithPrefix = prefixSet.flatMap{ prefix =>
          ontonotesCandidates.map{ c =>
            val target = prefix + " " + c.getSurfaceForm
            target.r.findAllIn(paragraph.context) -> target
          }
        }.filter{_._1.nonEmpty}.map{ case (a, t) =>
          println(s"(s, e) = (${a.start}, ${a.end})  /  text length :" + paragraph.contextTAOpt.get.text.length)
          val charStart = a.start
          val charEnd = a.end - 1
          val consStart = paragraphTokens.find{containsOffset(_, charStart) }.getOrElse(throw new Exception("didn't find the token"))
          val consEnd = paragraphTokens.find{containsOffset(_, charEnd) }.getOrElse(throw new Exception("didn't find the token"))
          new Constituent(t, "candidates", paragraph.contextTAOpt.get, consStart.getStartSpan, consEnd.getEndSpan)
        }

        println("constituentsWithPrefix = " + constituentsWithPrefix)
        candidates.++=:(constituentsWithPrefix)

        // WikiData
        if(candidates.isEmpty) {
          val wikiMentions = wikiMentionsInText.filter{ c =>
            WikiUtils.wikiAskQuery(c.getLabel, WikiDataProperties.person, WikiDataProperties.instanceOf, 5)
          }
          candidates.++=:(wikiMentions)
        }

        // organization
        if(candidates.isEmpty) {
          candidates.++=:(paragraphNerConsConll.filter(_.getLabel.contains("ORG")))
        }
      }

      // which trigger
      if(questionConstituentOpt.get._1.getSurfaceForm.toLowerCase.trim.contains("which ") ) {
        println("which trigger! ")

        // which location
        if(triggerTerm.contains("location")) {
          // NER-Conll
          def conllLocationFilter(in: String): Boolean = in.contains("LOC") || in.contains("ORG")
          candidates.++=:(paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))
          println("surface: " + paragraphNerConsConll)
          println("labels: " + paragraphNerConsConll.map(_.getLabel))
          println("conll filtered: " + paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))

          // NER-Ontonotes
          def ontonotesLocationFilter(in: String): Boolean = in.contains("LOC") || in.contains("ORG") || in.contains("GPE") || in.contains("FAC")
          val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontonotesLocationFilter(a.getLabel))
          candidates.++=:(ontonotesCandidates)
          println(paragraphNerConsOnto)
        }

        // which institute
        if(triggerTerm.contains("institute")) {
          // NER-Conll
          def conllLocationFilter(in: String): Boolean = in.contains("ORG")
          candidates.++=:(paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))
          println("surface: " + paragraphNerConsConll)
          println("labels: " + paragraphNerConsConll.map(_.getLabel))
          println("conll filtered: " + paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))

          // NER-Ontonotes
          def ontonotesLocationFilter(in: String): Boolean = in.contains("ORG")
          val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontonotesLocationFilter(a.getLabel))
          candidates.++=:(ontonotesCandidates)
          println(paragraphNerConsOnto)
        }

        // which entity
        if(triggerTerm.contains("entity")) {
          // NER-Conll
          def conllLocationFilter(in: String): Boolean = in.contains("ORG")
          candidates.++=:(paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))
          println("surface: " + paragraphNerConsConll)
          println("labels: " + paragraphNerConsConll.map(_.getLabel))
          println("conll filtered: " + paragraphNerConsConll.filter(a => conllLocationFilter(a.getLabel)))

          // NER-Ontonotes
          def ontonotesLocationFilter(in: String): Boolean = in.contains("ORG")
          val ontonotesCandidates = paragraphNerConsOnto.filter(a => ontonotesLocationFilter(a.getLabel))
          candidates.++=:(ontonotesCandidates)
          println(paragraphNerConsOnto)
        }

        // WikiData
        if(candidates.isEmpty) {
          val wikiMentions = wikiMentionsInText.filter { c =>
            WikiUtils.wikiAskQuery(c.getLabel, triggerTerm, WikiDataProperties.instanceOf, 5)
          }
          candidates.++=:(wikiMentions)
        }
      }

      /*
      if(questionConstituent.get.getSurfaceForm.contains(" age ")){
        println("age trigger! ")
      }
      var triggerTerm = questionConstituent.get.getSurfaceForm.split(" ").tail.mkString(" ")
      println("trigger term:  " + triggerTerm)
      val triggerWordCategories = WikiUtils.extractRelevantCategories(triggerTerm)
      println("triggerWordCategories: " + triggerWordCategories)
      val wikiMentionCategories = wikiMentionsInText.map{ c => c -> WikiUtils.extractCategoryOfWikipage(c.getSurfaceForm) }
      println("Paragraph categories: " + wikiMentionCategories.mkString("\n"))
      val wikiMentions = wikiMentionCategories.filter{case (c, categories) => categories.toSet.intersect(triggerWordCategories.toSet).nonEmpty}
      println("Mentions with intersections" + wikiMentions.map(_._1))
      */
    }
    println("Candidates = " + candidates.toSet)
  }

}
