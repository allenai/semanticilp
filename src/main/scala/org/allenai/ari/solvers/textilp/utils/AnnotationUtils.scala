package org.allenai.ari.solvers.textilp.utils

import java.util.Properties

import edu.illinois.cs.cogcomp.nlp.common.PipelineConfigurator._
import edu.illinois.cs.cogcomp.quant.driver.Quantifier
import edu.illinois.cs.cogcomp.saulexamples.nlp.TextAnnotationFactory
import org.allenai.ari.solvers.textilp.{Paragraph, Question, TopicGroup}

object AnnotationUtils {
  lazy val pipelineService = {
    val settings = new Properties()
    TextAnnotationFactory.disableSettings(settings, USE_SRL_NOM, USE_SRL_VERB,
      USE_STANFORD_DEP, USE_STANFORD_PARSE)
    TextAnnotationFactory.createPipelineAnnotatorService(settings)
  }

  lazy val quantifierAnnotator = new Quantifier()

  def annotateInstance(tg: TopicGroup): TopicGroup = {
    val annotatedParagraphs = tg.paragraphs.map{ p =>
      val annotatedContext = pipelineService.createBasicTextAnnotation("", "", p.context)
      val annotatedQuestions = p.questions.map{ q=>
        val ta = pipelineService.createBasicTextAnnotation("", "", q.questionText)
        Question(q.questionText, q.questionId, q.answers, Some(ta))
      }
      Paragraph(p.context, annotatedQuestions, Some(annotatedContext))
    }
    TopicGroup(tg.title, annotatedParagraphs)
  }
}
