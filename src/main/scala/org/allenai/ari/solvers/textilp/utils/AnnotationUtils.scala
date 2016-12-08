package org.allenai.ari.solvers.textilp.utils

import java.util.Properties

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation
import edu.illinois.cs.cogcomp.core.utilities.SerializationHelper
import edu.illinois.cs.cogcomp.core.utilities.configuration.Configurator
import edu.illinois.cs.cogcomp.nlp.common.PipelineConfigurator._
import edu.illinois.cs.cogcomp.quant.driver.Quantifier
import edu.illinois.cs.cogcomp.saulexamples.nlp.TextAnnotationFactory
import org.allenai.ari.solvers.textilp.{ Paragraph, Question, TopicGroup }
import org.allenai.common.cache.JsonQueryCache
import redis.clients.jedis.{ JedisPool, Protocol }

import spray.json.DefaultJsonProtocol._

/** a dummy redis client for when redis is not supposed to be used */
object DummyRedisClient extends JsonQueryCache[String]("", new JedisPool()) {
  override def get(key: String) = None
  override def put(key: String, value: String): Unit = {}
}

object AnnotationUtils {

  // redis cache for annotations
  lazy val synchronizedRedisClient = if (Constants.useRedisCachingForAnnotation) {
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
      if (withQuantifier) AnnotationUtils.quantifierAnnotator.addView(textAnnotation)
      synchronizedRedisClient.put(cacheKey, SerializationHelper.serializeToJson(textAnnotation))
      textAnnotation
    }
  }

  val pipelineService = {
    val settings = new Properties()
    viewsToDisable.foreach { v => settings.setProperty(v.key, Configurator.FALSE) }
    TextAnnotationFactory.createPipelineAnnotatorService(settings)
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
