package controllers

import javax.inject._

import models.StaticContent
import org.allenai.ari.solvers.bioProccess.ProcessBankReader
import org.allenai.ari.solvers.squad.CandidateGeneration
import org.allenai.ari.solvers.textilp.ResultJson
import org.allenai.ari.solvers.textilp.ResultJson._
import org.allenai.ari.solvers.textilp.solvers._
import org.allenai.ari.solvers.textilp.utils.{AnnotationUtils, SolverUtils}
import play.api.mvc._
import play.api.libs.json._

/** This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class SolveQuestion @Inject() extends Controller {

  lazy val salienceSolver = new SalienceSolver()
  lazy val luceneSolver = new LuceneSolver()
  lazy val slidingWindowSolver = new SlidingWindowSolver()
  lazy val annotationUtils = new AnnotationUtils()
  lazy val textilpSolver = new TextILPSolver(annotationUtils, verbose = true, params = SolverUtils.params)

  /** Create an Action to render an HTML page with a welcome message.
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index = Action {
    Ok(views.html.main("", "", "", "", StaticContent.initialFormContent, Json.toJson(ResultJson.emptyEntityRelation).toString))
  }

  def solve = Action(parse.json) { request =>
    val solverType = (request.body \ "solverType").as[JsString].value
    val question = (request.body \ "question").as[JsString].value
    val options = (request.body \ "options").as[JsString].value
    val snippet = ProcessBankReader.normalizeText((request.body \ "snippet").as[JsString].value)

    println("Options: " + options)

    val optionsPostProcessed = if (options.length < 2) {
      // it's empty; get the candidate options automatically
      val ta = annotationUtils.annotate(snippet)
      val generatedCandidates = CandidateGeneration.getCandidateAnswer(ta)
      println("Automatically extracted candidtes: " + generatedCandidates.mkString("//"))
      generatedCandidates.toSeq
    } else {
      options.split("(\\([A-Z]\\)|\\/\\/)").toSeq.map(_.trim).filter(_.nonEmpty)
    }

    println("optionsPostProcessed: " + optionsPostProcessed)


    println("snippet: " + snippet)
    val snippetPostprocessed = if (snippet.length < 5) {
      // it's empty; get it with elastic-search
      println("Asking the elastic-search . . . " + question)
      //val snippet = optionsPostProcessed.flatMap(focus => SolverUtils.extractParagraphGivenQuestionAndFocusWord(question, focus, 3)).mkString(" ")
      val rawSentences = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, optionsPostProcessed, 8).mkString(". ")
      annotationUtils.dropRedundantSentences(rawSentences)
    } else {
      snippet
    }
    println("snippetPostprocessed: " + snippetPostprocessed)

    println("solver type : " + solverType)
    val solverContent = if (solverType.toLowerCase.contains("salience")) {
      println("Calling salience . . . ")
      val (_, out) = salienceSolver.solve(question, optionsPostProcessed, snippetPostprocessed)
      println("Salience solver response . . .  ")
      println(out)
      out
    } else if (solverType.toLowerCase.contains("lucene")) {
      println("Calling lucene . . . ")
      val (_, out) = luceneSolver.solve(question, optionsPostProcessed, snippetPostprocessed)
      println("Lucene solver response . . .  ")
      println(out)
      out
    } else if (solverType.toLowerCase.contains("textilp")) {
      println("Calling te xtilp. . . ")
      val (_, out) = textilpSolver.solve(question, optionsPostProcessed, snippetPostprocessed)
//      val time = java.lang.System.currentTimeMillis()
//      TextILPSolver.sahandClient.useCache("sahandClient-" + time.toString)
//      annotationUtils.pipelineServerClient.useCaching("pipelineServerClient-" + time.toString)
//      annotationUtils.pipelineExternalAnnotatorsServerClient.useCaching("pipelineExternalAnnotatorsServerClient-" + time.toString)
//      annotationUtils.fillInBlankAnnotator.useCaching("fillInBlankAnnotator-" + time.toString)
      println("textilp solver response ..... ")
      println(out)
      out
    } else {
      throw new Exception("the solver not found :/")
    }
    println("Sending new resultls ")
    Ok(Json.toJson(solverContent).toString())
  }

  def getPrefilledQuestion(index: Int) = Action { request =>
    val question = StaticContent.getContentWithPrefilled(index).questionOpt.get.str
    val options = StaticContent.getContentWithPrefilled(index).questionOpt.get.questionChoice
    val snippet = StaticContent.getContentWithPrefilled(index).questionOpt.get.snippet.str
    Ok(views.html.main("", question, options, snippet, StaticContent.getContentWithPrefilled(index),
      Json.toJson(ResultJson.emptyEntityRelation).toString))
  }
}
