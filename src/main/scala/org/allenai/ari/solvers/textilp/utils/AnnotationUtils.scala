package org.allenai.ari.solvers.textilp.utils

import java.util.Properties

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.core.utilities.configuration.{Configurator, ResourceManager}
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator._
import edu.illinois.cs.cogcomp.pipeline.main.PipelineFactory
import edu.illinois.cs.cogcomp.quant.driver.Quantifier
import org.allenai.ari.solvers.textilp.{Paragraph, Question, TopicGroup}
import org.allenai.common.cache.JsonQueryCache
import redis.clients.jedis.{JedisPool, Protocol}
import spray.json.DefaultJsonProtocol._

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
  lazy val synchronizedRedisClient = if(false) { // if (Constants.useRedisCachingForAnnotation) {
    JsonQueryCache[String]("", "localhost", Protocol.DEFAULT_PORT, Protocol.DEFAULT_TIMEOUT)
  } else {
    // use the dummy client, which always returns None for any query (and not using any Redis)
    DummyRedisClient
  }

  val viewsToDisable = Set(USE_SRL_NOM, USE_SRL_VERB, USE_STANFORD_DEP, USE_STANFORD_PARSE)

  def annotate(string: String, withQuantifier: Boolean = true): TextAnnotation = {
    val cacheKey = "TextAnnotations:" + viewsToDisable.mkString("*") + withQuantifier + string
    val redisAnnotation = synchronizedRedisClient.get(cacheKey)
    if (redisAnnotation.isDefined) {
      SerializationHelper.deserializeFromJson(redisAnnotation.get)
    } else {
      val textAnnotation = pipelineService.createAnnotatedTextAnnotation("", "", string)
      if (withQuantifier) quantifierAnnotator.addView(textAnnotation)
      synchronizedRedisClient.put(cacheKey, SerializationHelper.serializeToJson(textAnnotation))
      textAnnotation
    }
  }

  val pipelineService = {
    val settings = new Properties()
    settings.setProperty("cacheDirectory", "annotation-cache-textilp")
    settings.setProperty("disableCache", Configurator.TRUE)
    viewsToDisable.foreach{ key => settings.setProperty(key.value, Configurator.FALSE) }
    val rm = new ResourceManager(settings)
    //viewsToDisable.foreach { v => settings.setProperty(v.key, Configurator.FALSE) }
    val config = new PipelineConfigurator().getConfig(rm)
    PipelineFactory.buildPipeline(config)
  }

  val quantifierAnnotator = new Quantifier()

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
}
