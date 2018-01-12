package org.allenai.ari.solvers.textilp.utils

import java.util
import java.util.Properties

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation
import edu.illinois.cs.cogcomp.core.io.caches.TextAnnotationMapDBHandler
import edu.illinois.cs.cogcomp.core.utilities.configuration.{ Configurator, ResourceManager }
import edu.illinois.cs.cogcomp.curator.{ CuratorConfigurator, CuratorFactory }
import edu.illinois.cs.cogcomp.pipeline.common.{ PipelineConfigurator, Stanford331Configurator }
import edu.illinois.cs.cogcomp.pipeline.common.PipelineConfigurator._
import edu.illinois.cs.cogcomp.pipeline.main.PipelineFactory
import edu.illinois.cs.cogcomp.pipeline.server.ServerClientAnnotator
import org.allenai.ari.solvers.textilp.solvers.TextILPSolver
import org.allenai.ari.solvers.textilp.{ Paragraph, Question, TopicGroup }

class AnnotationUtils() {

  val viewsToDisableAll = Set(USE_POS, USE_LEMMA,
    USE_SHALLOW_PARSE, USE_DEP, USE_NER_CONLL, USE_NER_ONTONOTES,
    USE_STANFORD_PARSE, USE_STANFORD_DEP, USE_SRL_VERB, USE_SRL_NOM,
    USE_QUANTIFIER)
  val viewsToDisable = Set(USE_SRL_NOM, USE_QUANTIFIER, USE_STANFORD_DEP)
  val viewsToAdd = Seq(ViewNames.POS, ViewNames.LEMMA, ViewNames.NER_CONLL, ViewNames.NER_ONTONOTES,
    ViewNames.SHALLOW_PARSE, ViewNames.PARSE_STANFORD, ViewNames.DEPENDENCY_STANFORD, ViewNames.SRL_VERB, ViewNames.SRL_PREP,
    ViewNames.SRL_COMMA /*, ViewNames.QUANTITIES*/ )

  lazy val globalAnnotationCache = new TextAnnotationMapDBHandler("allTheCacheTogether.db")

  lazy val fillInBlankAnnotator = new FillInBlankAnnotator

  // this is not used (or shouldn't be)
  lazy val pipelineService = {
    println("Starting to build the pipeline service . . . ")
    val settings = new Properties()
    settings.setProperty("disableCache", Configurator.TRUE)
    settings.setProperty("splitOnDash", Configurator.FALSE)
    settings.setProperty("stanfordMaxTimePerSentence", "1000000")
    viewsToDisableAll.foreach { key => settings.setProperty(key.value, Configurator.FALSE) }
    settings.setProperty(Stanford331Configurator.STFRD_MAX_SENTENCE_LENGTH.key, "1000")
    val rm = new ResourceManager(settings)
    val config = new PipelineConfigurator().getConfig(rm)
    val service = PipelineFactory.buildPipeline(config)
    println("Done with building the pipeline service . . . ")
    service
  }

  // used in experiments, but not in public release
  lazy val curatorService = {
    val settings = new Properties()
    settings.setProperty(CuratorConfigurator.DISABLE_CACHE.key, Configurator.FALSE)
    val rm = new ResourceManager(settings)
    val config = new CuratorConfigurator().getConfig(rm)
    CuratorFactory.buildCuratorClient(config)
  }

  lazy val pipelineServerClient = {
    val x = new ServerClientAnnotator()
    x.setUrl(Constants.cogcompAnnotatorServer, Constants.cogcompAnnotatorPort)
    x.setViewsAll(viewsToAdd.toArray)
    x.useCaching("remotePipelineCachingTextilp6.cache")
    x
  }

  lazy val pipelineServerClientWithBasicViews = {
    val x = new ServerClientAnnotator()
    x.setUrl(Constants.cogcompAnnotatorServer, Constants.cogcompAnnotatorPort)
    x.setViewsAll(Seq(ViewNames.SHALLOW_PARSE).toArray)
    x.useCaching("remotePipelineCachingTextilp-basicViews2.cache")
    x
  }

  lazy val pipelineExternalAnnotatorsServerClient = {
    val x = new ServerClientAnnotator()
    x.setUrl(Constants.externalAnnotatorsServer, Constants.externalAnnotatorsPort)
    x.setViewsAll(Array("SRL_VERB_PATH_LSTM", "STANFORD_COREF"))
    x.useCaching("externalAnnotations2.cache")
    x
  }

  def annotateWithCuratorAndSaveUnderName(string: String, newViewName: String, oldViewName: String, ta: TextAnnotation): TextAnnotation = {
    try {
      val set = new util.HashSet[String]()
      set.add(oldViewName)
      val ta1 = curatorService.createAnnotatedTextAnnotation("", "", string, set)
      ta.addView(newViewName, ta1.getView(oldViewName))
    } catch {
      case e: Exception => e.printStackTrace()
    }
    ta
  }

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

  import scala.collection.JavaConverters._

  def blankQuestionAnswerOptionNormalizer(answerOptionTA: TextAnnotation, blankQuestionTA: TextAnnotation, annotationUtils: AnnotationUtils): String = {
    val constant = "||||"
    val qToks = blankQuestionTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala
    val ansToks = answerOptionTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala
    val blankIdxOpt = qToks.zipWithIndex.find(_._1.getSurfaceForm.trim == "BLANK_").map(_._2)
    if (blankIdxOpt.isDefined) {
      val blankIdx = blankIdxOpt.get
      val simpleDrop = qToks.slice(0, blankIdx).map(_.getSurfaceForm).mkString("", " ", " ") +
        constant + qToks.slice(blankIdx + 1, qToks.length).map(_.getSurfaceForm).mkString(" ", " ", "")
      val filteredQuestion = if (blankIdx > 0 && qToks(blankIdx - 1).getLabel == "VP") {
        qToks.slice(0, blankIdx - 1).map(_.getSurfaceForm).mkString("", " ", " ") +
          constant + qToks.slice(blankIdx + 1, qToks.length).map(_.getSurfaceForm).mkString(" ", " ", "")
      } else if (blankIdx < qToks.length - 1 && qToks(blankIdx + 1).getLabel == "VP") {
        qToks.slice(0, blankIdx).map(_.getSurfaceForm).mkString("", " ", " ") +
          constant + qToks.slice(blankIdx + 2, qToks.length).map(_.getSurfaceForm).mkString(" ", " ", "")
      } else {
        simpleDrop
      }
      if (ansToks.last.getLabel == "VP" || ansToks.head.getLabel == "VP") {
        filteredQuestion.replace(constant, answerOptionTA.text)
      } else if (ansToks.length > 2 && ansToks(ansToks.length - 2).getLabel == "VP" &&
        (ansToks.last.getLabel == "PP" || ansToks.last.getLabel == "NP")) {
        filteredQuestion.replace(constant, answerOptionTA.text)
      } else if (ansToks.map(_.getLabel).contains("VP")) {
        filteredQuestion.replace(constant, answerOptionTA.text)
      } else {
        simpleDrop.replace(constant, answerOptionTA.text)
      }
    } else {
      blankQuestionTA.text.replace("BLANK_", answerOptionTA.text).dropRight(1).trim
    }
  }

  def dropRedundantSentences(str: String): String = {
    val ta = pipelineService.createBasicTextAnnotation("", "", str)
    ta.getView(ViewNames.SENTENCE).getConstituents.asScala.map { _.getSurfaceForm }.distinct.mkString(". ").
      replaceAll("\\.\\.\\.\\.", ".").replaceAll("\\.\\.\\.", ".").replaceAll("\\.\\.", ".")
  }

  def annotateWithEverything(input1: String, withFillInBlank: Boolean = false): TextAnnotation = synchronized {
    val input = SolverUtils.clearRedundantCharacters(input1)
    println("Annotating input: " + input)
    val ta = pipelineService.createBasicTextAnnotation("", "", input)
    if (globalAnnotationCache.contains(ta)) {
      val ta2 = globalAnnotationCache.getTextAnnotation(ta)
      if (withFillInBlank && !ta2.hasView(fillInBlankAnnotator.getViewName)) {
        ta2.addView(fillInBlankAnnotator)
        globalAnnotationCache.updateTextAnnotation(ta2)
      }
      ta2
    } else {
      println("--> normal views: ")
      val ta1 = pipelineServerClient.annotate(input)
      println(" --> external: ")
      pipelineExternalAnnotatorsServerClient.addView(ta1)
      println(" --> curator: ")
      if (Constants.useCurator) annotateWithCuratorAndSaveUnderName(ta1.text, TextILPSolver.curatorSRLViewName, ViewNames.SRL_VERB, ta1)
      println(" --> adding fill in the blanks ")
      if (withFillInBlank) ta1.addView(fillInBlankAnnotator)
      globalAnnotationCache.addTextAnnotation("", ta1)
      ta1
    }
  }

}
