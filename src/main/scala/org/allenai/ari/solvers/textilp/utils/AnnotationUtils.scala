package org.allenai.ari.solvers.textilp.utils

import java.io.File
import java.net.URLEncoder
import java.util.Properties

import edu.illinois.cs.cogcomp.annotation.AnnotatorServiceConfigurator
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{Constituent, TextAnnotation}
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.core.utilities.configuration.{Configurator, ResourceManager}
import edu.illinois.cs.cogcomp.curator.CuratorFactory
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator._
import edu.illinois.cs.cogcomp.pipeline.main.PipelineFactory
import edu.illinois.cs.cogcomp.saulexamples.nlp.QuestionTypeClassification.QuestionTypeAnnotator
import org.allenai.ari.solvers.squad.{CandidateGeneration, SQuADReader}
import org.allenai.ari.solvers.textilp.{Paragraph, Question, TopicGroup}
import org.allenai.common.cache.JsonQueryCache
import play.api.libs.json.{JsArray, JsNumber, Json}
import redis.clients.jedis.{JedisPool, Protocol}
import spray.json.DefaultJsonProtocol._

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
    JsonQueryCache[String]("", Constants.redisServer, Constants.redisPort, timeoutMillis = Constants.timeout)
  } else {
    // use the dummy client, which always returns None for any query (and not using any Redis)
    DummyRedisClient
  }

  val viewsToDisable = Set(USE_SRL_NOM, USE_SRL_VERB, USE_QUANTIFIER, USE_STANFORD_DEP)
  val viewsToAdd = Seq(ViewNames.POS, ViewNames.LEMMA, ViewNames.NER_CONLL, ViewNames.NER_ONTONOTES,
    ViewNames.SHALLOW_PARSE, ViewNames.PARSE_STANFORD /*, ViewNames.DEPENDENCY_STANFORD*/ /*, ViewNames.QUANTITIES*/ )

  lazy val pipelineService = {
    println("Starting to build the pipeline service . . . ")
    val settings = new Properties()
    //settings.setProperty("cacheDirectory", "annotation-cache-textilp")
    settings.setProperty("disableCache", Configurator.TRUE)
    settings.setProperty("splitOnDash", Configurator.FALSE)
    settings.setProperty("stanfordMaxTimePerSentence", "1000000")
    viewsToDisable.foreach { key => settings.setProperty(key.value, Configurator.FALSE) }
    settings.setProperty(PipelineConfigurator.STFRD_MAX_SENTENCE_LENGTH.key, "1000")
    val rm = new ResourceManager(settings)
    //viewsToDisable.foreach { v => settings.setProperty(v.key, Configurator.FALSE) }
    val config = new PipelineConfigurator().getConfig(rm)
    val service = PipelineFactory.buildPipeline(config)
    println("Done with building the pipeline service . . . ")
    service
  }

  lazy val curatorService = {
    //    val a = new AnnotatorServiceConfigurator().getDefaultConfig
    //    val settings = new Properties()
    //    settings.setProperty("cacheDirectory", "annotation-cache-textilp")
    //settings.setProperty("disableCache", Configurator.TRUE)
    //viewsToDisable.foreach{ key => settings.setProperty(key.value, Configurator.FALSE) }
    //    val rm = new ResourceManager(settings)
    //viewsToDisable.foreach { v => settings.setProperty(v.key, Configurator.FALSE) }
    //val config = new CuratorConfigurator().getConfig(rm)
    CuratorFactory.buildCuratorClient()
  }

  def annotateWithCuratorCorefAndCache(string: String): TextAnnotation = {
    val cacheKey = "*curatorWithCoref" + string
    val redisAnnotation = synchronizedRedisClient.get(cacheKey)
    if (redisAnnotation.isDefined) {
      SerializationHelper.deserializeFromJson(redisAnnotation.get)
    } else {
      val textAnnotation = curatorService.createBasicTextAnnotation("", "", string)
      curatorService.addView(textAnnotation, ViewNames.COREF)
      synchronizedRedisClient.put(cacheKey, SerializationHelper.serializeToJson(textAnnotation))
      textAnnotation
    }
  }

  def annotate(string: String): TextAnnotation = {
    val cacheKey = "*TextAnnotations" + viewsToDisable.mkString("*") + viewsToAdd.mkString("*") + string
    val redisAnnotation = synchronizedRedisClient.get(cacheKey)

    def anno() = {
      //      println("------")
      //      println(string)
      //val textAnnotation = pipelineService.createAnnotatedTextAnnotation("", "", string)
      val textAnnotation = pipelineService.createBasicTextAnnotation("", "", string)
      try {
        viewsToAdd.foreach { vu => pipelineService.addView(textAnnotation, vu) }
      } catch {
        case e: java.lang.NullPointerException =>
          println(s"Exception thrown . . . \nInput string: $string")
          e.printStackTrace()
      }
      //if (withQuantifier) quantifierAnnotator.addView(textAnnotation)
      synchronizedRedisClient.put(cacheKey, SerializationHelper.serializeToJson(textAnnotation))
      textAnnotation
    }

    if (redisAnnotation.isDefined) {
      try {
        SerializationHelper.deserializeFromJson(redisAnnotation.get)
      } catch {
        case e: java.lang.ArrayIndexOutOfBoundsException =>
          println("Exception in deserialization. Here is the sentence: " + string)
          println("And the full stacktrace: \n")
          e.printStackTrace()
          anno()
      }
    } else {
      anno()
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

  def processSQuADWithWikifierAndPutRedis(reader: SQuADReader) = {
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          val redisOutput = CandidateGeneration.wikifierRedis.get(p.context)
          if (redisOutput.isEmpty) {
            val ta = curatorService.createBasicTextAnnotation("", "", p.context)
            curatorService.addView(ta, ViewNames.WIKIFIER)
            val json = SerializationHelper.serializeToJson(ta)
            CandidateGeneration.wikifierRedis.put(ta.getText, json)
          }
          p.questions.foreach { q =>
            val redisOutput = CandidateGeneration.wikifierRedis.get(q.questionText)
            if (redisOutput.isEmpty) {
              val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
              curatorService.addView(ta, ViewNames.WIKIFIER)
              val json = SerializationHelper.serializeToJson(ta)
              CandidateGeneration.wikifierRedis.put(ta.getText, json)
            }
          }
        }
    }
  }

  /*  def verifyWikifierAnnotationsOnDisk(reader: SQuADReader) = {
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          val redisOutput = CandidateGeneration.wikifierRedis.get(p.context)
          require(redisOutput.isDefined)
          p.questions.foreach { q =>
            val redisOutput = CandidateGeneration.wikifierRedis.get(q.questionText)
            require(redisOutput.isDefined)
          }
        }
    }
  }*/

  def processSQuADWithWikifier(reader: SQuADReader) = {
    import java.io._
    val pw = new PrintWriter(new File("squadTrain_wikifier.txt"))
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

  // processes questions and paragraphs together (hence "jointly")
  def processSQuADWithWikifierJointly(reader: SQuADReader) = {
    import java.io._
    val pw = new PrintWriter(new File("squadTrain_wikifier.txt"))
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          val jointText = (p.context + p.questions.map { _.questionText }).mkString(" ")
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
          val redisOutput = CandidateGeneration.wikifierRedis.get(p.context)
          require(redisOutput.isDefined)
          p.questions.foreach { q =>
            //val ta = curatorService.createBasicTextAnnotation("", "", q.questionText)
            //curatorService.addView(ta, ViewNames.WIKIFIER)
            //val json = SerializationHelper.serializeToJson(ta)
            //wikifierRedis.put(ta.getText, json)
            val redisOutput = CandidateGeneration.wikifierRedis.get(q.questionText)
            require(redisOutput.isDefined)
          }
        }
    }
  }

  def annotateVerbSRLwithRemoteServer(ta: TextAnnotation) = {
    val query = Constants.pipelineServer(ta.text, ViewNames.SRL_VERB)
    val html = Source.fromURL(query)
    val jsonString = html.mkString
    val taNew = SerializationHelper.deserializeFromJson(jsonString)
    ta.addView(ViewNames.SRL_VERB, taNew.getView(ViewNames.SRL_VERB))
    println("ta.getAvailableViews: " + ta.getAvailableViews)
//    val perOptionResponses = (json \\ "views").head.as[JsArray]
//    println(perOptionResponses.value.length)
//    perOptionResponses.value.foreach{ v =>
//      println("v: " + v)
//      println("------")
//    }

//    perOptionResponses.value.map { perOptionResponse =>
//      val confidence = (perOptionResponse \ "confidence").as[JsNumber].value.toDouble
//      val selection = (perOptionResponse \ "selection" \ "multipleChoice" \ "key").as[String]
//      val focus = (perOptionResponse \ "selection" \ "multipleChoice" \ "focus").as[String]
//      focus -> confidence
//    }
    //TODO: implement this.
  }

  lazy val bioProcessCorefMap = {
    // reading the saved coref-textAnnotations from disk
    val separator = "<><><><>"
    // reads in the bio-process data
    val lines = Source.fromFile(new File("bioProcessCoref.txt")).getLines().mkString.split(separator)
    lines.map { l =>
      val ta = SerializationHelper.deserializeFromJson(l)
      ta.text -> ta
    }.toMap
  }

}
