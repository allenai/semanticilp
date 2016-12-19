package org.allenai.ari.solvers.textilp.solvers

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.infer.ilp.{GurobiHook, OJalgoHook}
import org.allenai.ari.solvers.textilp.alignment.{AlignmentFunction, KeywordTokenizer}
import org.allenai.ari.solvers.textilp._
import org.allenai.ari.solvers.textilp.ilpsolver.IlpStatus.IlpStatusOptimal
import org.allenai.ari.solvers.textilp.ilpsolver._
import org.allenai.ari.solvers.textilp.utils.AnnotationUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

object TextILPSolver {
  val epsilon = 0.001
  val oneActiveSentenceConstraint = true

  val scienceTermsBoost = false
  val interSentenceAlignments = false
  val stopWords = false
  val essentialTerms = false
  val minQuestionToParagraphAlignmentScore = 0.0
  val minParagraphToQuestionAlignmentScore = 0.0
  val minInterSentenceAlignmentScore = 0.0
  val activeSentenceAlignmentCoeff = -1.0 // penalizes extra sentence usage
  val constituentAlignmentCoeff = -0.1
  val activeScienceTermBoost = 1d
  val minActiveParagraphConstituentAggrAlignment = 0.1
  val minActiveQuestionConstituentAggrAlignment = 0.1
  val minAlignmentWhichTerm = 0.6d
  val minPConsToPConsAlignment = 0.6
  val minPConsTQChoiceAlignment = 0.2
  val whichTermAddBoost = 1.5d
  val whichTermMulBoost = 1d

  val essentialTermsMturkConfidenceThreshold = 0.9
  val essentialClassifierConfidenceThreshold = 0.9
  val essentialTermsFracToCover = 1.0 // a number in [0,1]
  val essentialTermsSlack = 1 // a non-negative integer
  val essentialTermWeightScale = 1.0
  val essentialTermWeightBias = 0.0
  val essentialTermMinimalSetThreshold = 0.8
  val essentialTermMaximalSetThreshold = 0.2
  val essentialTermMinimalSetTopK = 3
  val essentialTermMaximalSetBottomK = 0
  val essentialTermMinimalSetSlack = 1
  val essentialTermMaximalSetSlack = 0

  lazy val keywordTokenizer = KeywordTokenizer.Default
  lazy val aligner = new AlignmentFunction("Entailment", 0.1, keywordTokenizer, useRedisCache = false, useContextInRedisCache = false)
}

class TextILPSolver(annotationUtils: AnnotationUtils) extends TextSolver {
  def solve(question: String, options: Seq[String], snippet: String): (Seq[Int], EntityRelationResult) = {
    val ilpSolver = new ScipSolver("textILP", ScipParams.Default)
//    val ilpSolver = new IllinoisInference(new OJalgoHook)
//    val ilpSolver = new IllinoisInference(new GurobiHook)
    val answers = options.map(o => Answer(o, -1))
//    println("Tokenizing question .... ")
    val qTA = annotationUtils.pipelineService.createBasicTextAnnotation("", "", question)
    if(question.trim.nonEmpty) {
      annotationUtils.pipelineService.addView(qTA, ViewNames.SHALLOW_PARSE)
    }
    val q = Question(question, "", answers, Some(qTA))
//    println("Tokenizing paragraph .... ")
    val pTA = annotationUtils.pipelineService.createBasicTextAnnotation("", "", snippet) // AnnotationUtils.annotate(snippet, withQuantifier = false)
    if(snippet.trim.nonEmpty) {
      annotationUtils.pipelineService.addView(pTA, ViewNames.SHALLOW_PARSE)
    }
    val p = Paragraph(snippet, Seq(q), Some(pTA))
    createILPModel(q, p, ilpSolver, TextILPSolver.aligner)
  }

  def createILPModel[V <: IlpVar](
    q: Question,
    p: Paragraph,
    ilpSolver: IlpSolver[V, _],
    alignmentFunction: AlignmentFunction
  ): (Seq[Int], EntityRelationResult) = {

    println("starting to create the model  . . . ")

    require(q.qTAOpt.isDefined, "the annotatins for the question is not defined")
    require(p.contextTAOpt.isDefined, "the annotatins for the paragraph is not defined")
    val qTA = q.qTAOpt.getOrElse(throw new Exception("The annotation for the question not found . . . "))
    val pTA = p.contextTAOpt.getOrElse(throw new Exception("The annotation for the paragraph not found . . . "))
    val qTokens = if(qTA.hasView(ViewNames.SHALLOW_PARSE)) qTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala else Seq.empty
    val pTokens = if(pTA.hasView(ViewNames.SHALLOW_PARSE)) pTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala else Seq.empty

    ilpSolver.setAsMaximization()

    // create questionToken-paragraphToken alignment edges
    val questionParagraphAlignments = for {
      qCons <- qTokens
      pCons <- pTokens
      score = alignmentFunction.scoreCellCell(qCons.getSurfaceForm, pCons.getSurfaceForm)
      x = ilpSolver.createBinaryVar("", score)
    } yield (qCons, pCons, x)

    // create paragraphToken-answerOption alignment edges
    val paragraphAnswerAlignments = for {
      pCons <- pTokens
      ans <- q.answers
      score = alignmentFunction.scoreCellCell(pCons.getSurfaceForm, ans.answerText)
      x = ilpSolver.createBinaryVar("", score)
    } yield (pCons, ans, x)

    // high-level variables
    // active answer options
    val activeAnswerOptions = for {
      ans <- q.answers
      x = ilpSolver.createBinaryVar("", 0.0)
    } yield (ans, x)

    def getVariablesConnectedToOption(ans: Answer): Seq[V] = {
      paragraphAnswerAlignments.filter { case (_, ansTmp, _) => ansTmp == ans }.map(_._3)
    }

    def getVariablesConnectedToParagraphToken(c: Constituent): Seq[V] = {
      questionParagraphAlignments.filter { case (_, cTmp, _) => cTmp == c }.map(_._3) ++
        paragraphAnswerAlignments.filter { case (cTmp, _, _) => cTmp == c }.map(_._3)
    }

    def getVariablesConnectedToParagraphSentence(sentenceId: Int): Seq[V] = {
      pTokens.filter(_.getSentenceId == sentenceId).flatMap(getVariablesConnectedToParagraphToken)
    }

    // variable must be active if anything connected to it is active
    activeAnswerOptions.foreach {
      case (ans, x) =>
        val connectedVariables = getVariablesConnectedToOption(ans)
        val allVars = connectedVariables :+ x
        val coeffs = Seq.fill(connectedVariables.length)(-1.0) :+ 1.0
        ilpSolver.addConsBasicLinear("activeOptionVar", allVars, coeffs, None, Some(0.0))
        connectedVariables.foreach { connectedVar =>
          val vars = Seq(connectedVar, x)
          val coeffs = Seq(1.0, -1.0)
          ilpSolver.addConsBasicLinear("activeOptionVar", vars, coeffs, None, Some(0.0))
        }
    }

    // active sentences for the paragraph
    val activeParagraphConstituents = for {
      t <- pTokens
      x = ilpSolver.createBinaryVar("", 0.0)
    } yield (t, x)

    // the paragraph token is active if anything connected to it is active
    activeParagraphConstituents.foreach {
      case (ans, x) =>
        val connectedVariables = getVariablesConnectedToParagraphToken(ans)
        val allVars = connectedVariables :+ x
        val coeffs = Seq.fill(connectedVariables.length)(-1.0) :+ 1.0
        ilpSolver.addConsBasicLinear("activeOptionVar", allVars, coeffs, None, Some(0.0))
        connectedVariables.foreach { connectedVar =>
          val vars = Seq(connectedVar, x)
          val coeffs = Seq(1.0, -1.0)
          ilpSolver.addConsBasicLinear("activeParagraphConsVar", vars, coeffs, None, Some(0.0))
        }
    }

    // active sentences for the paragraph
    val activeSentences = for {
      s <- 0 until pTA.getNumberOfSentences
      x = ilpSolver.createBinaryVar("", 0.0)
    } yield (s, x)

//    val activeSentenceVariablePerConstituet = {
//      val paragraphConsToVar = activeSentences.toMap
//      activeParagraphConstituents.map{ case (c, v) => c -> paragraphConsToVar(c.getSentenceId) }
//    }.toMap

    // the sentence variable is active if anything connected to it is active
    activeSentences.foreach {
      case (ans, x) =>
        val connectedVariables = getVariablesConnectedToParagraphSentence(ans)
        val allVars = connectedVariables :+ x
        val coeffs = Seq.fill(connectedVariables.length)(-1.0) :+ 1.0
        ilpSolver.addConsBasicLinear("activeOptionVar", allVars, coeffs, None, Some(0.0))
        connectedVariables.foreach { connectedVar =>
          val vars = Seq(connectedVar, x)
          val coeffs = Seq(1.0, -1.0)
          ilpSolver.addConsBasicLinear("activeParagraphConsVar", vars, coeffs, None, Some(0.0))
        }
    }

    // constraints
    // alignment to only one option, i.e. there must be only one single active option
    val activeAnsVars = activeAnswerOptions.map { case (ans, x) => x }
    val activeAnsVarsCoeffs = Seq.fill(activeAnsVars.length)(1.0)
    ilpSolver.addConsBasicLinear("onlyOneActiveOption", activeAnsVars, activeAnsVarsCoeffs, Some(1.0), Some(1.0))

    // have at most one active sentence
    val (_, sentenceVars) = activeSentences.unzip
    val sentenceVarsCoeffs = Seq.fill(sentenceVars.length)(1.0)
    ilpSolver.addConsBasicLinear("activeParagraphConsVar", sentenceVars, sentenceVarsCoeffs, Some(0.0), Some(1.0))

    // sparsity parameters
    // alignment is preferred for lesser sentences

    println("created the ilp model. Now solving it  . . . ")

//    println("Number of binary variables: " + ilpSolver.getNBinVars)
//    println("Number of continuous variables: " + ilpSolver.getNContVars)
//    println("Number of integer variables: " + ilpSolver.getNIntVars)
//    println("Number of constraints: " + ilpSolver.getNConss)

    // solving and extracting the answer
    ilpSolver.solve()

    println("Done solving the model  . . . ")

//    // extracting the solution
//    val questionAlignments = qTokens.map { c => c -> TermAlignment(c.getSurfaceForm) }.toMap
//    val choiceAlignments = q.answers.map { c => c -> TermAlignment(c.answerText) }.toMap
//    val paragraphAlignments = pTokens.map { c => c -> TermAlignment(c.getSurfaceForm) }.toMap

//    var iter = 0
//    questionParagraphAlignments.foreach {
//      case (c1, c2, x) =>
//        if (ilpSolver.getSolVal(x) > 1.0 - epsilon) {
//          questionAlignments(c1).alignmentIds.+=(iter)
//          paragraphAlignments(c2).alignmentIds.+=(iter)
//          iter = iter + 1
//        }
//    }

    val selectedIndex = if(ilpSolver.getStatus == IlpStatusOptimal) {
      activeAnswerOptions.zipWithIndex.collect { case ((ans, x), idx) if ilpSolver.getSolVal(x) > 1.0 - TextILPSolver.epsilon => idx }
    }
    else {
      Seq.empty
    }

    val questionBeginning = "Question: "
    val paragraphBeginning = "|Paragraph: "
    val questionString = questionBeginning + q.questionText
    val choiceString = "|Options: " + q.answers.zipWithIndex.map{case (ans, key) => s" (${key+1}) " + ans.answerText}.mkString(" ")
    val paragraphString = paragraphBeginning + p.context

    val entities = ArrayBuffer[Entity]()
    val relations = ArrayBuffer[Relation]()
    var eIter = 0
    var rIter = 0

    val entityMap = scala.collection.mutable.Map[(Int, Int), String]()
    val relationSet = scala.collection.mutable.Set[(String, String)]()

    questionParagraphAlignments.foreach {
      case (c1, c2, x) =>
        if (ilpSolver.getSolVal(x) > 1.0 - TextILPSolver.epsilon) {
//          val qBeginIndex = questionString.indexOf(c1.getSurfaceForm)
          val qBeginIndex = questionBeginning.length + c1.getStartCharOffset
          val qEndIndex = qBeginIndex + c1.getSurfaceForm.length
          val span1 = (qBeginIndex, qEndIndex)
          val t1 = if(!entityMap.contains(span1)) {
            val t1 = "T" + eIter
            eIter = eIter + 1
            entities += Entity(t1, c1.getSurfaceForm, Seq(span1))
            entityMap.put(span1, t1)
            t1
          }
          else {
             entityMap(span1)
          }

//          val pBeginIndex = paragraphString.indexOf(c2.getSurfaceForm) + questionString.length
          val pBeginIndex = c2.getStartCharOffset + questionString.length + paragraphBeginning.length
          val pEndIndex = pBeginIndex + c2.getSurfaceForm.length
          val span2 = (pBeginIndex, pEndIndex)
          val t2 = if(!entityMap.contains(span2)) {
            val t2 = "T" + eIter
            eIter = eIter + 1
            entities += Entity(t2, c2.getSurfaceForm, Seq(span2))
            entityMap.put(span2, t2)
            t2
          }
          else {
            entityMap(span2)
          }

          if(!relationSet.contains((t1, t2))) {
            relations += Relation("R" + rIter, t1, t2, ilpSolver.getVarObjCoeff(x))
            rIter = rIter + 1
            relationSet.add((t1, t2))
          }
        }
    }

//    paragraphAnswerAlignments.foreach {
//      case (c1, c2, x) =>
//        if (ilpSolver.getSolVal(x) > 1.0 - epsilon) {
//          paragraphAlignments(c1).alignmentIds.+=(iter)
//          choiceAlignments(c2).alignmentIds.+=(iter)
//          iter = iter + 1
//        }
//    }

    paragraphAnswerAlignments.foreach {
      case (c1, c2, x) =>
        if (ilpSolver.getSolVal(x) > 1.0 - TextILPSolver.epsilon) {
//          val pBeginIndex = paragraphString.indexOf(c1.getSurfaceForm) + questionString.length
//          val pEndIndex = pBeginIndex + c1.getSurfaceForm.length
          val pBeginIndex = c1.getStartCharOffset + questionString.length + paragraphBeginning.length
          val pEndIndex = pBeginIndex + c1.getSurfaceForm.length
          val span1 = (pBeginIndex, pEndIndex)
          val t1 = if(!entityMap.contains(span1)) {
            val t1 = "T" + eIter
            entities += Entity(t1, c1.getSurfaceForm, Seq(span1))
            entityMap.put(span1, t1)
            eIter = eIter + 1
            t1
          } else {
            entityMap(span1)
          }

          val oBeginIndex = choiceString.indexOf(c2.answerText) + questionString.length + paragraphString.length
          val oEndIndex = oBeginIndex + c2.answerText.length
          val span2 = (oBeginIndex, oEndIndex)
          val t2 = if(!entityMap.contains(span2)) {
            val t2 = "T" + eIter
            entities += Entity(t2, c2.answerText, Seq(span2))
            eIter = eIter + 1
            entityMap.put(span2, t2)
            t2
          }
          else {
            entityMap(span2)
          }

          if(!relationSet.contains((t1, t2))) {
            relations += Relation("R" + rIter, t1, t2, ilpSolver.getVarObjCoeff(x))
            rIter = rIter + 1
            relationSet.add((t1, t2))
          }
        }
    }

    println("returning the answer  . . . ")

//    val alignmentResult = AlignmentResults(
//      questionAlignments.values.toList,
//      choiceAlignments.values.toList,
//      paragraphAlignments.values.toList
//    )

    val erView = EntityRelationResult(questionString + paragraphString + choiceString, entities, relations)

    selectedIndex -> erView
  }
}
