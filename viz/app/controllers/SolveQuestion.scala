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
  val params = TextIlpParams(
    activeQuestionTermWeight = 0.33,
    alignmentScoreDiscount = 0.0, // not used
    questionCellOffset = -0.4, // tuned
    paragraphAnswerOffset = -0.4, // tuned
    firstOrderDependencyEdgeAlignments = 0.0,
    activeSentencesDiscount = -2.5, // tuned
    activeParagraphConstituentsWeight = 0.0, // tuned
    minQuestiontTermsAligned = 1,
    maxQuestiontTermsAligned = 3,
    minQuestiontTermsAlignedRatio = 0.1,
    maxQuestiontTermsAlignedRatio = 0.65,
    maxActiveSentences = 2,
    longerThan1TokenAnsPenalty = 0.0,
    longerThan2TokenAnsPenalty = 0.0,
    longerThan3TokenAnsPenalty = 0.02,

    // Answer Options: sparsity
    moreThan1AlignmentAnsPenalty = -0.3,
    moreThan2AlignmentAnsPenalty = -0.5,
    moreThan3AlignmentAnsPenalty = -0.7,

    exactMatchMinScoreValue = 0.76,
    exactMatchMinScoreDiff = 0.15,
    exactMatchSoftWeight = 0.0,

    // Question: sparsity
    moreThan1AlignmentToQuestionTermPenalty = -0.3,
    moreThan2AlignmentToQuestionTermPenalty = -0.4,
    moreThan3AlignmentToQuestionTermPenalty = -0.5,

    // Paragraph: proximity inducing
    activeDist1WordsAlignmentBoost = 0.0,
    activeDist2WordsAlignmentBoost = 0.0,
    activeDist3WordsAlignmentBoost = 0.0,

    // Paragraph: sparsity
    maxNumberOfWordsAlignedPerSentence = 8,
    maxAlignmentToRepeatedWordsInParagraph = 3,
    moreThan1AlignmentToParagraphTokenPenalty = 0.0,
    moreThan2AlignmentToParagraphTokenPenalty  = 0.0,
    moreThan3AlignmentToParagraphTokenPenalty = 0.0,

    // Paragraph: intra-sentence alignment
    coreferenceWeight = 0.0,
    intraSentenceAlignmentScoreDiscount = 0.0,
    entailmentWeight = 0.0,
    srlAlignmentWeight = 0.0
  )
  lazy val textilpSolver = new TextILPSolver(annotationUtils, verbose = true, params = params)

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
    println("Snippet: " + snippet)

    val optionsPostProcessed = if (options.length < 2) {
      // it's empty; get the candidate options automatically
      val ta = annotationUtils.annotate(snippet)
      val generatedCandidates = CandidateGeneration.getCandidateAnswer(ta)
      println("Automatically extracted candidtes: " + generatedCandidates.mkString("//"))
      generatedCandidates.toSeq
    } else {
      options.split("//").toSeq
    }

    val snippetPostprocessed = if (snippet.length < 2) {
      // it's empty; get it with elastic-search
      println("Asking the elastic-search . . . ")
      //val snippet = optionsPostProcessed.flatMap(focus => SolverUtils.extractParagraphGivenQuestionAndFocusWord(question, focus, 3)).mkString(" ")
      val snippet = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, optionsPostProcessed, 8).mkString(" ")
      println(snippet)
      snippet
    } else {
      snippet
    }

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
      println("Calling textilp. . . ")
      val (_, out) = textilpSolver.solve(question, optionsPostProcessed, snippetPostprocessed)
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
    Ok(views.html.main("", question, options, snippet, StaticContent.getContentWithPrefilled(index), Json.toJson(ResultJson.emptyEntityRelation).toString))
  }

}
