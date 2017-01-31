package org.allenai.ari.solvers.textilp.solvers

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.Constituent
import edu.illinois.cs.cogcomp.infer.ilp.{GurobiHook, OJalgoHook}
import org.allenai.ari.solvers.textilp.alignment.{AlignmentFunction, KeywordTokenizer}
import org.allenai.ari.solvers.textilp._
import org.allenai.ari.solvers.textilp.ilpsolver._
import org.allenai.ari.solvers.textilp.utils.AnnotationUtils

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

import org.allenai.ari.solvers.bioProccess.ProcessBankReader._

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
  val trueFalseThreshold = 5.5 // this has to be tuned

  lazy val keywordTokenizer = KeywordTokenizer.Default

  def getMaxScore(qCons: Seq[Constituent], pCons: Seq[Constituent]): Double = {
    val scoreList = qCons.flatMap{ qC =>
      pCons.map{ pC =>
        val score2 = offlineAligner.scoreCellCell(pC.getSurfaceForm, qC.getSurfaceForm)
        ///println(s"pC: $pC / qC: $qC / score: $score2")
        score2
      }
    }
    //println("maxScore: " + maxScore)
    //println("-----------------------")
    scoreList.max
//    scoreList.sum / scoreList.length
  }

  def getAvgScore(qCons: Seq[Constituent], pCons: Seq[Constituent]): Double = {
    val scoreList = qCons.flatMap{ qC =>
      pCons.map{ pC =>
        val score2 = offlineAligner.scoreCellCell(pC.getSurfaceForm, qC.getSurfaceForm)
        ///println(s"pC: $pC / qC: $qC / score: $score2")
        score2
      }
    }
    //println("maxScore: " + maxScore)
    //println("-----------------------")
    scoreList.sum / scoreList.length
  }

  lazy val offlineAligner = new AlignmentFunction("Entailment", 0.2, TextILPSolver.keywordTokenizer, useRedisCache = false, useContextInRedisCache = false)
}

class TextILPSolver(annotationUtils: AnnotationUtils, alignmentThreshold: Double, verbose: Boolean = true, activeQuestionTermWeight: Double) extends TextSolver {

  lazy val aligner = new AlignmentFunction("Entailment", alignmentThreshold, TextILPSolver.keywordTokenizer, useRedisCache = false, useContextInRedisCache = false)

  def solve(question: String, options: Seq[String], snippet: String): (Seq[Int], EntityRelationResult) = {
    val ilpSolver = new ScipSolver("textILP", ScipParams.Default)
//    val ilpSolver = new IllinoisInference(new OJalgoHook)
//    val ilpSolver = new IllinoisInference(new GurobiHook)
    val answers = options.map(o => Answer(o, -1, Some(annotationUtils.annotate(o))))
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
    createILPModel(q, p, ilpSolver, aligner)
  }

  def createILPModel[V <: IlpVar](
    q: Question,
    p: Paragraph,
    ilpSolver: IlpSolver[V, _],
    alignmentFunction: AlignmentFunction
  ): (Seq[Int], EntityRelationResult) = {

    if(verbose) println("starting to create the model  . . . ")
    val isTrueFalseQuestion = q.isTrueFalse

    val isTemporalQuestions = q.isTemporal
    require(q.qTAOpt.isDefined, "the annotatins for the question is not defined")
    require(p.contextTAOpt.isDefined, "the annotatins for the paragraph is not defined")
    val qTA = q.qTAOpt.getOrElse(throw new Exception("The annotation for the question not found . . . "))
    val pTA = p.contextTAOpt.getOrElse(throw new Exception("The annotation for the paragraph not found . . . "))
    val qTokens = if(qTA.hasView(ViewNames.SHALLOW_PARSE)) qTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala else Seq.empty
    val pTokens = if(pTA.hasView(ViewNames.SHALLOW_PARSE)) pTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala else Seq.empty

    ilpSolver.setAsMaximization()

    // whether to create the model with the tokenized version of the answer options
    val tokenizeAnswers = true //if(q.isTemporal) true else false
    val aTokens = if(tokenizeAnswers) {
      q.answers.map(_.aTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.map(_.getSurfaceForm))
    }
    else {
      q.answers.map(a => Seq(a.answerText))
    }

    // create questionToken-paragraphToken alignment edges
    val questionParagraphAlignments = for {
      qCons <- qTokens
      pCons <- pTokens
      score = alignmentFunction.scoreCellCell(qCons.getSurfaceForm, pCons.getSurfaceForm)
      x = ilpSolver.createBinaryVar("", score)
    } yield (qCons, pCons, x)

    // create paragraphToken-answerOption alignment edges
    val paragraphAnswerAlignments = if(!isTrueFalseQuestion) {
        // create only multiple nodes at each answer option
        for {
          pCons <- pTokens
          ansIdx <- aTokens.indices
          ansConsIdx <- aTokens(ansIdx).indices
          ansConsString = aTokens(ansIdx).apply(ansConsIdx)
          score = alignmentFunction.scoreCellCell(pCons.getSurfaceForm, ansConsString)
          x = ilpSolver.createBinaryVar("", score)
        } yield (pCons, ansIdx, ansConsIdx, x)
    } else {
      List.empty
    }

    // high-level variables
    // active answer options
    val activeAnswerOptions = if(!isTrueFalseQuestion) {
      for {
        ansIdx <- q.answers.indices
        x = ilpSolver.createBinaryVar("activeAnsOptId" + ansIdx, 0.0)
      } yield (ansIdx, x)
    }
    else {
      List.empty
    }

    def getVariablesConnectedToOption(ansIdx: Int): Seq[V] = {
      paragraphAnswerAlignments.filter { case (_, ansTmp, _, _) => ansTmp == ansIdx }.map(_._4)
    }

    def getVariablesConnectedToParagraphToken(c: Constituent): Seq[V] = {
      questionParagraphAlignments.filter { case (_, cTmp, _) => cTmp == c }.map(_._3) ++
        paragraphAnswerAlignments.filter { case (cTmp, _, _, _) => cTmp == c }.map(_._4)
    }

    def getVariablesConnectedToParagraphSentence(sentenceId: Int): Seq[V] = {
      pTokens.filter(_.getSentenceId == sentenceId).flatMap(getVariablesConnectedToParagraphToken)
    }

    def getVariablesConnectedToQuestionToken(qCons: Constituent): Seq[V] = {
      questionParagraphAlignments.filter { case (cTmp, _, _) => cTmp == qCons }.map(_._3)
    }

    // Answer option must be active if anything connected to it is active
    activeAnswerOptions.foreach {
      case (ansIdx, x) =>
        val connectedVariables = getVariablesConnectedToOption(ansIdx)
        val allVars = connectedVariables :+ x
        val coeffs = Seq.fill(connectedVariables.length)(-1.0) :+ 1.0
        ilpSolver.addConsBasicLinear("activeOptionVarImplesOneActiveConnectedEdge", allVars, coeffs, None, Some(0.0))
        connectedVariables.foreach { connectedVar =>
          val vars = Seq(connectedVar, x)
          val coeffs = Seq(1.0, -1.0)
          ilpSolver.addConsBasicLinear("activeConnectedEdgeImpliesOneAnswerOption", vars, coeffs, None, Some(0.0))
        }
    }

    // active paragraph constituent
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
      x = ilpSolver.createBinaryVar("activeSentence:" + s, -2.0)
    } yield (s, x)
    // the paragraph constituent variable is active if anything connected to it is active
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

    // active questions cons
    val activeQuestionConstituents = for {
      t <- qTokens
      weight = activeQuestionTermWeight /*if(q.questionText.isWhatDoesItDo) {
        0.2
      }
      else {
        activeQuestionTermWeight
      }*/
      x = ilpSolver.createBinaryVar("activeQuestionCons", weight) //TODO: add weight for this?
    } yield (t, x)
    // the question token is active if anything connected to it is active
    activeQuestionConstituents.foreach {
      case (c, x) =>
        val connectedVariables = getVariablesConnectedToQuestionToken(c)
        val allVars = connectedVariables :+ x
        val coeffs = Seq.fill(connectedVariables.length)(-1.0) :+ 1.0
        ilpSolver.addConsBasicLinear("activeQuestionIneq1", allVars, coeffs, None, Some(0.0))
        connectedVariables.foreach { connectedVar =>
          val vars = Seq(connectedVar, x)
          val coeffs = Seq(1.0, -1.0)
          ilpSolver.addConsBasicLinear("activeQuestionIneq2", vars, coeffs, None, Some(0.0))
        }
    }

    // constraints
    // alignment to only one option, i.e. there must be only one single active option
    if(activeAnswerOptions.nonEmpty) {
      val activeAnsVars = activeAnswerOptions.map { case (ans, x) => x }
      val activeAnsVarsCoeffs = Seq.fill(activeAnsVars.length)(1.0)
      ilpSolver.addConsBasicLinear("onlyOneActiveOption", activeAnsVars, activeAnsVarsCoeffs, Some(1.0), Some(1.0))
    }

    // have at most one active sentence
    if(true) {
      val (_, sentenceVars) = activeSentences.unzip
      val sentenceVarsCoeffs = Seq.fill(sentenceVars.length)(1.0)
      ilpSolver.addConsBasicLinear("maxActiveParagraphConsVar", sentenceVars, sentenceVarsCoeffs, Some(0.0), Some(2.0))
    } else {
      // do nothing
    }

    // sparsity parameters
    // alignment is preferred for lesser sentences

    // for result questions ...
    if(q.questionText.isForCResultQuestion && activeAnswerOptions.length == 2) {
      def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
        pCons.map{ c =>
          TextILPSolver.getMaxScore(qCons, Seq(c))
        }.zipWithIndex.maxBy(_._1)._2
      }

      val qIdx = getClosestIndex(qTokens, pTokens)
      val ans1Cons = q.answers(0).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
      val ans2Cons = q.answers(1).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
      val a1Idx = getClosestIndex(ans1Cons, pTokens)
      val a2Idx = getClosestIndex(ans2Cons, pTokens)
      // at least one of the answers happens before the question
      if (a1Idx < qIdx && a2Idx > qIdx ) {
        // a2 should be the answer
        ilpSolver.addConsBasicLinear("resultReasoning-secondAnswerMustBeCorrect", Seq(activeAnswerOptions(1)._2), Seq(1.0), Some(1.0), None)
      }
      if (a2Idx < qIdx && a1Idx > qIdx) {
        // a1 should be the answer
        ilpSolver.addConsBasicLinear("resultReasoning-firstAnswerMustBeCorrect", Seq(activeAnswerOptions(0)._2), Seq(1.0), Some(1.0), None)
      }
    }

    if(q.questionText.isWhatDoesItDo && activeAnswerOptions.length == 2) {
        def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
          pCons.map { c =>
            TextILPSolver.getAvgScore(qCons, Seq(c))
          }.zipWithIndex.maxBy(_._1)._2
        }
        val qIdx = getClosestIndex(qTokens, pTokens)
        val ans1Cons = q.answers(0).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
        val ans2Cons = q.answers(1).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
        val a1Idx = getClosestIndex(ans1Cons, pTokens)
        val a2Idx = getClosestIndex(ans2Cons, pTokens)
        // one before, one after: after is the anser
        if (a1Idx < qIdx && a2Idx > qIdx) {
          // at least one of the answers happens before the question
          // the second one is the answer
          ilpSolver.addConsBasicLinear("resultReasoning-secondAnswerMustBeCorrect", Seq(activeAnswerOptions(1)._2), Seq(1.0), Some(1.0), None)
        }
        if (a1Idx > qIdx && a2Idx < qIdx) {
          // at least one of the answers happens before the question
          // the first one is the answer
          ilpSolver.addConsBasicLinear("resultReasoning-firstAnswerMustBeCorrect", Seq(activeAnswerOptions(0)._2), Seq(1.0), Some(1.0), None)
        }
        // both after: closer is the answer
        if (a1Idx > qIdx && a2Idx > qIdx) {
          // at least one of the answers happens before the question
          if (a2Idx < a1Idx) {
            // the second one is the answer
            ilpSolver.addConsBasicLinear("resultReasoning-secondAnswerMustBeCorrect", Seq(activeAnswerOptions(1)._2), Seq(1.0), Some(1.0), None)
          }
          if (a2Idx > a1Idx ) {
            // the first one is the answer
            ilpSolver.addConsBasicLinear("resultReasoning-firstAnswerMustBeCorrect", Seq(activeAnswerOptions(0)._2), Seq(1.0), Some(1.0), None)
          }
        }
      }

    // intra-sentence alignments
    // any sentences (that are at most k-sentences apart; k = 2 for now) can be aligned together.
/*

    val maxIntraSentenceDistance = 2
    val intraSentenceAlignments = for{
      beginSentence <- 0 until (pTA.getNumberOfSentences - maxIntraSentenceDistance)
      offset <- 0 until maxIntraSentenceDistance
      endSentence = beginSentence + offset
      x = ilpSolver.createBinaryVar(s"interSentenceAlignment/$beginSentence/$endSentence", 0.0)
    } yield (beginSentence, endSentence, x)
    // co-reference
*/


    // alignment between the constituents

    // use at least k constituents in the question
    // TODO: make this parameterized
    val (_, questionVars) = activeQuestionConstituents.unzip
    val questionVarsCoeffs = Seq.fill(questionVars.length)(1.0)
    ilpSolver.addConsBasicLinear("activeQuestionConsVar", questionVars, questionVarsCoeffs, Some(1.0), Some(3.0))

    // if anything comes after " without " it should be aligned definitely
    // example: What would happen without annealing?
    if(q.questionText.contains(" without ")) {
      if(verbose) println(" >>> Adding constraint to use the term after `without`")
      val withoutTok = qTokens.filter(_.getSurfaceForm == "without").head
      if(verbose) println("withoutTok: " + withoutTok)
      val after = qTokens.filter(c => c.getStartSpan > withoutTok.getStartSpan).minBy(_.getStartSpan)
      if(verbose) println("after: " + after)
      val afterVariableOpt = activeQuestionConstituents.collectFirst{case (c, v) if c == after => v}
      if(verbose) println("afterVariableOpt = " + afterVariableOpt)
      afterVariableOpt match {
        case Some(afterVariable) =>
          ilpSolver.addConsBasicLinear("termAfterWithoutMustBeAligned", Seq(afterVariable), Seq(1.0), Some(1.0), None)
        case None =>  // do nothing
      }
    }

    if(verbose) println("created the ilp model. Now solving it  . . . ")
//    println("Number of binary variables: " + ilpSolver.getNBinVars)
//    println("Number of continuous variables: " + ilpSolver.getNContVars)
//    println("Number of integer variables: " + ilpSolver.getNIntVars)
//    println("Number of constraints: " + ilpSolver.getNConss)

    // solving and extracting the answer
    ilpSolver.solve()

    if(verbose) println("Done solving the model  . . . ")

    if(verbose) {
      activeAnswerOptions.foreach {
        case (ansIdx, x) =>
          println("============================ \nAnsIdx: " + ansIdx)
          val connectedVariables = getVariablesConnectedToOption(ansIdx).toList
          println(s" ## Variables connect to ansIndex ")
          println(connectedVariables.map { x => ilpSolver.getSolVal(x).toString }.toSet)
      }
    }

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

    def stringifyVariableSequence(seq: Seq[(Int, V)]): String = {
      seq.map{ case(id, x) => "id: " + id + " : " + ilpSolver.getSolVal(x) }.mkString(" / ")
    }

    def stringifyVariableSequence3(seq: Seq[(Constituent, V)])(implicit d: DummyImplicit): String = {
      seq.map{ case(id, x) => "id: " + id.getSurfaceForm + " : " + ilpSolver.getSolVal(x) }.mkString(" / ")
    }

    def stringifyVariableSequence2(seq: Seq[(Constituent, Constituent, V)])(implicit d: DummyImplicit, d2: DummyImplicit): String = {
      seq.map{ case(c1, c2, x) => "c1: " + c1.getSurfaceForm + ", c2: " + c2.getSurfaceForm + " -> " + ilpSolver.getSolVal(x) }.mkString(" / ")
    }

    def stringifyVariableSequence4(seq: Seq[(Constituent, Int, Int, V)]): String = {
      seq.map{ case(c, i, j, x) => "c: " + c.getSurfaceForm + ", ansIdx: " + i + ", ansConsIdx: " + j + " -> " + ilpSolver.getSolVal(x) }.mkString(" / ")
    }

    if(ilpSolver.getStatus == IlpStatusOptimal) {
      if(verbose) println("Primal score: " + ilpSolver.getPrimalbound)
      val trueIdx = q.trueIndex
      val falseIdx = q.falseIndex
      val selectedIndex = if(isTrueFalseQuestion) {
        if(ilpSolver.getPrimalbound > TextILPSolver.trueFalseThreshold ) Seq(trueIdx) else Seq(falseIdx)
      }
      else {
        if(verbose) println(">>>>>>> not true/false . .. ")
        activeAnswerOptions.zipWithIndex.collect { case ((ans, x), idx) if ilpSolver.getSolVal(x) > 1.0 - TextILPSolver.epsilon => idx }
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

            // val pBeginIndex = paragraphString.indexOf(c2.getSurfaceForm) + questionString.length
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
        case (c1, ansIdx, ansConsIdx, x) =>
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

            val ansString = aTokens(ansIdx)(ansConsIdx)
            val ansswerBeginIdx = choiceString.indexOf(q.answers(ansIdx).answerText)
            val oBeginIndex = choiceString.indexOf(ansString, ansswerBeginIdx) + questionString.length + paragraphString.length
            val oEndIndex = oBeginIndex + ansString.length
            val span2 = (oBeginIndex, oEndIndex)
            val t2 = if(!entityMap.contains(span2)) {
              val t2 = "T" + eIter
              entities += Entity(t2, ansString, Seq(span2))
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

      if(isTrueFalseQuestion) {
        // add the answer option span manually
        selectedIndex.foreach{ idx =>
          val ansText = q.answers(idx).answerText
          val oBeginIndex = choiceString.indexOf(ansText) + questionString.length + paragraphString.length
          val oEndIndex = oBeginIndex + ansText.length
          val span2 = (oBeginIndex, oEndIndex)
          val t2 = if(!entityMap.contains(span2)) {
            val t2 = "T" + eIter
            entities += Entity(t2, ansText, Seq(span2))
            eIter = eIter + 1
            entityMap.put(span2, t2)
            t2
          }
          else {
            entityMap(span2)
          }
        }
      }

      if(verbose) println("returning the answer  . . . ")

      //    val alignmentResult = AlignmentResults(
      //      questionAlignments.values.toList,
      //      choiceAlignments.values.toList,
      //      paragraphAlignments.values.toList
      //    )

      val solvedAnswerLog = "activeAnswerOptions: " + stringifyVariableSequence(activeAnswerOptions) +
        "  activeQuestionConstituents: " + stringifyVariableSequence3(activeQuestionConstituents) +
        "  questionParagraphAlignments: " + stringifyVariableSequence2(questionParagraphAlignments) +
        "  paragraphAnswerAlignments: " + stringifyVariableSequence4(paragraphAnswerAlignments) +
        "  aTokens: " + aTokens.toString

      val erView = EntityRelationResult(questionString + paragraphString + choiceString, entities, relations,
        confidence = ilpSolver.getPrimalbound, log = solvedAnswerLog)
      selectedIndex -> erView
    }
    else {
      if(verbose) println("Not optimal . . . ")
      if(verbose) println("Status is not optimal. Status: "  + ilpSolver.getStatus)
      // if the program is not solver, say IDK
      Seq.empty -> EntityRelationResult("INFEASIBLE", List.empty, List.empty)
    }
  }

}
