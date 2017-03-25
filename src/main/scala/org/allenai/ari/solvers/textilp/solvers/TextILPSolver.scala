package org.allenai.ari.solvers.textilp.solvers

import java.net.URL

import edu.cmu.meteor.scorer.{MeteorConfiguration, MeteorScorer}
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{Constituent, TextAnnotation}
import org.allenai.ari.controller.questionparser.{FillInTheBlankGenerator, QuestionParse}
import org.allenai.ari.solvers.squad.CandidateGeneration
import org.simmetrics.StringMetric
import org.simmetrics.metrics.StringMetrics

import scala.collection.mutable
import org.allenai.ari.solvers.bioProccess.ProcessBankReader._
import org.allenai.ari.solvers.textilp._
import org.allenai.ari.solvers.textilp.alignment.{AlignmentFunction, KeywordTokenizer}
import org.allenai.ari.solvers.textilp.ilpsolver._
import org.allenai.ari.solvers.textilp.utils.{AnnotationUtils, SolverUtils}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

object TextILPSolver {
  val epsilon = 0.001
  val oneActiveSentenceConstraint = true

  val scienceTermsBoost = false
  val interSentenceAlignments = false
  val stopWords = false
  val essentialTerms = false
  val minInterSentenceAlignmentScore = 0.0
  val activeSentenceAlignmentCoeff = -1.0
  // penalizes extra sentence usage
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
  val essentialTermsFracToCover = 1.0
  // a number in [0,1]
  val essentialTermsSlack = 1
  // a non-negative integer
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


  val config = new MeteorConfiguration()
  config.setParaFileURL(new URL("file:/Users/daniel/ideaProjects/TextILP/src/main/resources/data/paraphrase-en.gz"))
  config.setWordFileURL(new URL("file:/Users/daniel/ideaProjects/meteor/src/main/resources/function/"))
  val meteroScorer = new MeteorScorer(config)

  // fill-in-blank generator
  lazy val fitbGenerator = FillInTheBlankGenerator.mostRecent

  def getMaxScore(qCons: Seq[Constituent], pCons: Seq[Constituent]): Double = {
    val scoreList = qCons.flatMap { qC =>
      pCons.map { pC =>
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
    val scoreList = qCons.flatMap { qC =>
      pCons.map { pC =>
        val score2 = offlineAligner.scoreCellCell(pC.getSurfaceForm, qC.getSurfaceForm)
        ///println(s"pC: $pC / qC: $qC / score: $score2")
        score2
      }
    }
    //println("maxScore: " + maxScore)
    //println("-----------------------")
    scoreList.sum / scoreList.length
  }

  lazy val offlineAligner = new AlignmentFunction("Entailment", 0.2,
    TextILPSolver.keywordTokenizer, useRedisCache = false, useContextInRedisCache = false)
}

case class TextIlpParams(
                          activeQuestionTermWeight: Double,
                          alignmentScoreDiscount: Double,
                          questionCellOffset: Double,
                          paragraphAnswerOffset: Double,
                          firstOrderDependencyEdgeAlignments: Double,
                          activeParagraphConstituentsWeight: Double,

                          minQuestionTermsAligned: Int,
                          maxQuestionTermsAligned: Int,
                          minQuestionTermsAlignedRatio: Double,
                          maxQuestionTermsAlignedRatio: Double,

                          activeSentencesDiscount: Double,
                          maxActiveSentences: Int,

                          longerThan1TokenAnsPenalty: Double,
                          longerThan2TokenAnsPenalty: Double,
                          longerThan3TokenAnsPenalty: Double,

                          exactMatchMinScoreValue: Double,
                          exactMatchMinScoreDiff: Double,
                          exactMatchSoftWeight: Double, // supposed to be a positive value

                          meteorExactMatchMinScoreValue: Double,
                          meteorExactMatchMinScoreDiff: Double,

                          minQuestionToParagraphAlignmentScore: Double,
                          minParagraphToQuestionAlignmentScore: Double,

                          // Answers: sparsity
                          moreThan1AlignmentAnsPenalty: Double,
                          moreThan2AlignmentAnsPenalty: Double,
                          moreThan3AlignmentAnsPenalty: Double,

                          // Question: sparsity
                          moreThan1AlignmentToQuestionTermPenalty: Double,
                          moreThan2AlignmentToQuestionTermPenalty: Double,
                          moreThan3AlignmentToQuestionTermPenalty: Double,

                          // Paragraph: proximity inducing
                          activeDist1WordsAlignmentBoost: Double,
                          activeDist2WordsAlignmentBoost: Double,
                          activeDist3WordsAlignmentBoost: Double,

                          // Paragraph: sparsity
                          maxNumberOfWordsAlignedPerSentence: Int,
                          maxAlignmentToRepeatedWordsInParagraph: Int,
                          moreThan1AlignmentToParagraphTokenPenalty: Double,
                          moreThan2AlignmentToParagraphTokenPenalty: Double,
                          moreThan3AlignmentToParagraphTokenPenalty: Double,

                          // Paragraph: intra-sentence alignment
                          coreferenceWeight: Double,
                          intraSentenceAlignmentScoreDiscount: Double,
                          entailmentWeight: Double,
                          srlAlignmentWeight: Double,
                          scieneTermBoost: Double
                        )

class TextILPSolver(annotationUtils: AnnotationUtils,
                    verbose: Boolean = true, params: TextIlpParams,
                    useRemoteAnnotation: Boolean = true) extends TextSolver {

  lazy val aligner = new AlignmentFunction("Entailment", 0.0,
    TextILPSolver.keywordTokenizer, useRedisCache = false, useContextInRedisCache = false)

  def solve(question: String, options: Seq[String], snippet: String): (Seq[Int], EntityRelationResult) = {
//    println("Snippet: " + snippet)
    val ilpSolver = new ScipSolver("textILP", ScipParams.Default)
    //    val ilpSolver = new IllinoisInference(new OJalgoHook)
    //    val ilpSolver = new IllinoisInference(new GurobiHook)
    val answers = options.map { o =>
      val ansTA = try {
        if(useRemoteAnnotation) {
//          annotationUtils.annotateViewLwithRemoteServer(ViewNames.SHALLOW_PARSE, ansTA)
//          annotationUtils.annotateViewLwithRemoteServer(ViewNames.DEPENDENCY_STANFORD, ansTA)
          Some(annotationUtils.pipelineServerClient.annotate(o))
        } else {
          val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "", o)
          annotationUtils.pipelineService.addView(ta, ViewNames.SHALLOW_PARSE)
          annotationUtils.pipelineService.addView(ta, ViewNames.DEPENDENCY_STANFORD)
          Some(ta)
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
          None
      }
      Answer(o, -1, ansTA)
    }
    val qTA = if (question.trim.nonEmpty) {
      if(useRemoteAnnotation) {
        //annotationUtils.annotateViewLwithRemoteServer(ViewNames.SHALLOW_PARSE, qTA)
//        annotationUtils.annotateViewLwithRemoteServer(qTA)
        val clientTa = annotationUtils.pipelineServerClient.annotate(question)
        clientTa.addView(annotationUtils.fillInBlankAnnotator)
        Some(clientTa)
      } else {
        val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "", question)
        annotationUtils.pipelineService.addView(ta, ViewNames.SHALLOW_PARSE)
        Some(ta)
      }
    }
    else {
      println("Question string is empty . . . ")
      println("question: " + question)
      println("snippet: " + snippet)
      None
    }
    val q = Question(question, "", answers, qTA)
    val pTA = if (snippet.trim.nonEmpty) {
      try {
        if(useRemoteAnnotation) {
//          annotationUtils.annotateViewLwithRemoteServer(ViewNames.SHALLOW_PARSE, pTA)
//          annotationUtils.annotateViewLwithRemoteServer(ViewNames.DEPENDENCY_STANFORD, pTA)
          //annotationUtils.annotateViewLwithRemoteServer(pTA)
          val clientTa = annotationUtils.pipelineServerClient.annotate(snippet)
          Some(clientTa)
        } else {
          val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "", snippet)
          // AnnotationUtils.annotate(snippet, withQuantifier = false)
          annotationUtils.pipelineService.addView(ta, ViewNames.SHALLOW_PARSE)
          annotationUtils.pipelineService.addView(ta, ViewNames.DEPENDENCY_STANFORD)
          Some(ta)
        }
      }
      catch {
        case e: Exception =>
          e.printStackTrace()
          None
      }
    }
    else {
      println("Paragraph String is empty ..... ")
      println("question: " + question)
      println("snippet: " + snippet)
      None
    }
    val p = Paragraph(snippet, Seq(q), pTA)
    //createILPModel(q, p, ilpSolver, aligner)
    // SRLSolverV1(q, p, aligner)
    // MeteorSolver(q, p, aligner)
    // WhatDoesItDoSolver(q, p)
    // SimilarityMetricSolver(q, p)
    // CauseResultRules(q, p)
  }

  // what (does|do|can) .* do
  def WhatDoesItDoSolver(q: Question, p: Paragraph): (Seq[Int], EntityRelationResult) = {
    val qTokens = if (q.qTAOpt.get.hasView(ViewNames.SHALLOW_PARSE)) q.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala else Seq.empty
    val pTokens = if (p.contextTAOpt.get.hasView(ViewNames.SHALLOW_PARSE)) p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala else Seq.empty

    val selected = if (q.questionText.isWhatDoesItDo && q.answers.length == 2) {
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
        Seq(1)
      }
      else if (a1Idx > qIdx && a2Idx < qIdx) {
        // at least one of the answers happens before the question
        // the first one is the answer
        Seq(0)
      } else if (a1Idx > qIdx && a2Idx > qIdx) { // both after: closer is the answer
        if (a2Idx < a1Idx) { // at least one of the answers happens before the question
          // the second one is the answer
          Seq(1)
        }
        else if (a2Idx > a1Idx) {
          // the first one is the answer
          Seq(0)
        }
        else {
          Seq.empty
        }
      }
      else {
        Seq.empty
      }
    }
    else {
      Seq.empty
    }
    selected.distinct -> EntityRelationResult()
  }

  def SimilarityMetricSolver(q: Question, p: Paragraph): (Seq[Int], EntityRelationResult) = {
    def compareSimilairityInWndow(p: Paragraph, questionString: String, windowSize: Int, metric: StringMetric): Double = {
      val paragraphTokens = p.contextTAOpt.get.getTokens
      val paragraphSize = paragraphTokens.length
      if(paragraphSize > windowSize) {
        (0 until paragraphSize - windowSize).map { startIdx =>
          val endIdx = startIdx + windowSize
          val paragraphWindow = paragraphTokens.slice(startIdx, endIdx).mkString(" ")
          metric.compare(paragraphWindow, questionString)
        }.max
      } else {
        0.0
      }
    }

    // exact match: if there is a good match between the question
    val qparse = QuestionParse.constructFromString(q.questionText)
    val metric = StringMetrics.qGramsDistance()
    val fitbQuestionStrOpt = TextILPSolver.fitbGenerator.generateFITB(qparse).map(_.text)
    val selected = fitbQuestionStrOpt match {
      case Some(x) =>
        val indexScorePairs = q.answers.indices.map { idx =>
          val str = x.replace("BLANK_", q.answers(idx).answerText).dropRight(1).toLowerCase
          idx -> compareSimilairityInWndow(p, str, q.qTAOpt.get.getTokens.length +
            q.answers(idx).aTAOpt.get.getTokens.length, metric)
        }
        val sortedScores = indexScorePairs.sortBy(-_._2).take(2)
        val maxS = sortedScores(0)._2
        val secondMaxS = sortedScores(1)._2
        val maxIdx = sortedScores(0)._1
        if (maxS > params.exactMatchMinScoreValue && maxS - secondMaxS >= params.exactMatchMinScoreDiff) {
          // maxIdx should be the answer
          Seq(maxIdx)
        }
        else  {
          Seq.empty
        }
      // Daniel: Commented our because it didn't seem to help
      /*else {
          // if not hard constraint, do soft weighting
          indexScorePairs.foreach { case (idx, score) =>
            require(score * params.exactMatchSoftWeight >= 0, "exactMatchSoftWeighting should be positive")
            val x = ilpSolver.createBinaryVar("exactMatchSoftWeightingAns" + idx, score * params.exactMatchSoftWeight)
            // if answer option is zero, x should be zero; note that the soft weight is positive
            ilpSolver.addConsBasicLinear(s"directAnswerSoftWeight-$idx",
              Seq(x, activeAnswerOptions(idx)._2), Seq(1.0, -1.0), None, Some(0.0))
          }
        }*/
      case None => Seq.empty
    }
    selected -> EntityRelationResult()
  }

  def CauseResultRules(q: Question, p: Paragraph): (Seq[Int], EntityRelationResult) = {
    // for result questions ...
    val answered = if (q.questionText.isForCResultQuestion && q.answers.length == 2) {
      def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
        pCons.map { c =>
          TextILPSolver.getAvgScore(qCons, Seq(c))
        }.zipWithIndex.maxBy(_._1)._2
      }

      val pCons = p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
      val qCons = q.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
      val qIdx = getClosestIndex(qCons, pCons)
      val ans1Cons = q.answers(0).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
      val ans2Cons = q.answers(1).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
      val a1Idx = getClosestIndex(ans1Cons, pCons)
      val a2Idx = getClosestIndex(ans2Cons, pCons)

      // at least one of the answers happens before the question
      if (a1Idx < qIdx && a2Idx > qIdx + 6) {
        // a2 should be the answer
        Seq(1)
      }
      else if (a2Idx < qIdx && a1Idx > qIdx + 6) {
        // a1 should be the answer
        Seq(0)
      }
      else {
        Seq.empty
      }
    }
    else {
      Seq.empty
    }
    answered -> EntityRelationResult()
  }

  def MeteorSolver(q: Question, p: Paragraph, alignmentFunction: AlignmentFunction): (Seq[Int], EntityRelationResult) = {
    def compareSimilairityInWndowBySentence(p: Paragraph, questionString: String): Double = {
      val paragraphTokensPerSentence = p.contextTAOpt.get.getTokens.zipWithIndex.map{ case (tok, tokId) =>
        val sentId = p.contextTAOpt.get.getSentenceId(tokId)
        (tok, sentId)
      }.groupBy(_._2)
      println("paragraphTokensPerSentence: ")
      println(paragraphTokensPerSentence)
      paragraphTokensPerSentence.map { case (sentId, toks) =>
        val paragraphWindow = toks.map(_._1).mkString(" ")
        val stats = TextILPSolver.meteroScorer.getMeteorStats(paragraphWindow, questionString)
        stats.score
      }.max
    }

    // exact match with Meteor
    val qparse = QuestionParse.constructFromString(q.questionText)
    // TODO: do it only once per question
    val fitbQuestionStrOpt = TextILPSolver.fitbGenerator.generateFITB(qparse).map(_.text)
    val selectedAnswers = fitbQuestionStrOpt match {
      case Some(x) =>
        val indexScorePairs = q.answers.indices.map { idx =>
          val str = x.replace("BLANK_", q.answers(idx).answerText).dropRight(1).toLowerCase
          idx -> compareSimilairityInWndowBySentence(p, str)
        }
        val sortedScores = indexScorePairs.sortBy(-_._2).take(2)
        val maxS = sortedScores(0)._2
        val secondMaxS = sortedScores(1)._2
        val maxIdx = sortedScores(0)._1
        if (maxS >= params.meteorExactMatchMinScoreValue && maxS - secondMaxS >= params.meteorExactMatchMinScoreDiff ) {
          // maxIdx should be the answer
          Seq(maxIdx)
        }
        else {
          Seq.empty
        }
      case None => Seq.empty
    }

    selectedAnswers.distinct -> EntityRelationResult()
  }

  def SRLSolverV2(q: Question, p: Paragraph): (Seq[Int], EntityRelationResult) = {
    val fillInBlank = q.qTAOpt.get.getView(annotationUtils.fillInBlankAnnotator.getViewName).getConstituents.get(0).getLabel
    val fillInBlankTA = annotationUtils.pipelineServerClient.annotate(fillInBlank)
    q.answers.foreach{ ans =>
      val questionAndAnsOption = annotationUtils.blankQuestionAnswerOptionNormalizer(ans.aTAOpt.get, fillInBlankTA, annotationUtils)
    }

    SRLSolverV1(q, p)
  }

  def SRLSolverV1(q: Question, p: Paragraph): (Seq[Int], EntityRelationResult) = {
    val selectedAnswers = if(q.qTAOpt.get.hasView(ViewNames.SRL_VERB) && p.contextTAOpt.get.hasView(ViewNames.SRL_VERB)) {
      val qSRLView = q.qTAOpt.get.getView(ViewNames.SRL_VERB)
      val pSRLView = p.contextTAOpt.get.getView(ViewNames.SRL_VERB)
      val qSRLCons = qSRLView.getConstituents.asScala
      val pSRLCons = pSRLView.getConstituents.asScala

      // does question-srl contain any question term?
      val cons = qSRLCons.filter(c => CandidateGeneration.questionTerms.contains(c.getSurfaceForm.toLowerCase))
      if (verbose) println(">>>>>> cons: " + cons)
      if (cons.nonEmpty) {
        if (cons.size > 1) {
          println("More than two cons overlapping with the question term. Choosing the head . . . ")
        }
        if (verbose) println("question: " + q.questionText)
        val qArgLabel = cons.head.getLabel
        if (verbose) println(">>>>>>> qArgLabel: " + qArgLabel)
        val qSource = cons.head.getIncomingRelations.get(0).getSource
        if (verbose) println(">>>>>>>> " + qSource)

        def getLabel(pred: Constituent): String = pred.getLabel + pred.getAttribute("SenseNumber") + pred.getAttribute("predicate")

        val qSourceLabel = getLabel(qSource)
        if (verbose) println(">>>>>>>> " + qSourceLabel)
        val pParagraph = pSRLCons.filter(c => getLabel(c) == qSourceLabel)
        val pArgCons = pParagraph.flatMap { pred =>
          pred.getOutgoingRelations.asScala.map(_.getTarget).filter(_.getLabel == qArgLabel)
        }
        if (verbose) println("pArgCons: " + pArgCons)

        // check if any of the any of the selected constituents overlap with one of the answers
        def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
          pCons.map { c =>
            TextILPSolver.getAvgScore(qCons, Seq(c))
          }.zipWithIndex.maxBy(_._1)._2
        }

        val pCons = p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
        val results = q.answers.map { ans =>
          val ansCons = ans.aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
          val ansParagraphIdx = getClosestIndex(ansCons, pCons)
          val ansSpan = pCons(ansParagraphIdx).getSpan
          if (verbose) println("ansCons: " + ansCons)
          if (verbose) println("pCons(ansParagraphIdx): " + pCons(ansParagraphIdx))
          if (pArgCons.exists { pArg => pArg.getSpan.getFirst <= ansSpan.getFirst && pArg.getSpan.getSecond >= ansSpan.getSecond }) 1 else 0
        }
        if (verbose) println("results: " + results)
        if (results.sum == 1) {
          // i.e. only one of the answer options has overlap
          results.zipWithIndex.flatMap { case (v, idx) =>
            if (v == 1) {
              println(s"SRL alignment: Setting the answer $idx to be active")
              Seq(idx)
            }
            else {
              Seq.empty
            }
          }
        }
        else {
          Seq.empty
        }
      }
      else {
        Seq.empty
      }
    }
    else {
      Seq.empty
    }

    selectedAnswers.distinct -> EntityRelationResult()
  }

  def matchPredicatesAndArguments(qTA: TextAnnotation, pTA: TextAnnotation, ans: Answer): Boolean = {
    if(qTA.hasView(ViewNames.SRL_VERB) && pTA.hasView(ViewNames.SRL_VERB)) {
      val qSRLView = qTA.getView(ViewNames.SRL_VERB)
      val pSRLView = pTA.getView(ViewNames.SRL_VERB)
      val qSRLCons = qSRLView.getConstituents.asScala
      val pSRLCons = pSRLView.getConstituents.asScala

      // does question-srl contain any question term?
      val cons = qSRLCons.filter(c => CandidateGeneration.questionTerms.contains(c.getSurfaceForm.toLowerCase))
      if (verbose) println(">>>>>> cons: " + cons)
      if (cons.nonEmpty) {
        if (cons.size > 1) {
          println("More than two cons overlapping with the question term. Choosing the head . . . ")
        }
        if (verbose) println("question: " + qTA.text)
        val qArgLabel = cons.head.getLabel
        if (verbose) println(">>>>>>> qArgLabel: " + qArgLabel)
        val qSource = cons.head.getIncomingRelations.get(0).getSource
        if (verbose) println(">>>>>>>> " + qSource)

        def getLabel(pred: Constituent): String = pred.getLabel + pred.getAttribute("SenseNumber") + pred.getAttribute("predicate")

        val qSourceLabel = getLabel(qSource)
        if (verbose) println(">>>>>>>> " + qSourceLabel)
        val pParagraph = pSRLCons.filter(c => getLabel(c) == qSourceLabel)
        val pArgCons = pParagraph.flatMap { pred =>
          pred.getOutgoingRelations.asScala.map(_.getTarget).filter(_.getLabel == qArgLabel)
        }
        if (verbose) println("pArgCons: " + pArgCons)

        // check if any of the any of the selected constituents overlap with one of the answers
        def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
          pCons.map { c =>
            TextILPSolver.getAvgScore(qCons, Seq(c))
          }.zipWithIndex.maxBy(_._1)._2
        }

        val pCons = pTA.getView(ViewNames.SHALLOW_PARSE).asScala.toList
        val ansCons = ans.aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
        val ansParagraphIdx = getClosestIndex(ansCons, pCons)
        val ansSpan = pCons(ansParagraphIdx).getSpan
        if (pArgCons.exists { pArg => pArg.getSpan.getFirst <= ansSpan.getFirst && pArg.getSpan.getSecond >= ansSpan.getSecond }) true else false
      }
      else {
        false
      }
    }
    else {
      false
    }
  }

  def createILPModel[V <: IlpVar](
                                   q: Question,
                                   p: Paragraph,
                                   ilpSolver: IlpSolver[V, _],
                                   alignmentFunction: AlignmentFunction
                                 ): (Seq[Int], EntityRelationResult) = {


    val modelCreationStart = System.currentTimeMillis()

    if (verbose) println("starting to create the model  . . . ")
    val isTrueFalseQuestion = q.isTrueFalse

    val useSRL = true

//    if(useSRL) {
//      annotationUtils.annotateVerbSRLwithRemoteServer(p.contextTAOpt.get)
//      annotationUtils.annotateVerbSRLwithRemoteServer(q.qTAOpt.get)
//    }

    val isTemporalQuestions = q.isTemporal
    require(q.qTAOpt.isDefined, "the annotatins for the question is not defined")
    require(p.contextTAOpt.isDefined, "the annotatins for the paragraph is not defined")
    val qTA = q.qTAOpt.getOrElse(throw new Exception("The annotation for the question not found . . . "))
    val pTA = p.contextTAOpt.getOrElse(throw new Exception("The annotation for the paragraph not found . . . "))
    val qTokens = if (qTA.hasView(ViewNames.SHALLOW_PARSE)) qTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala else Seq.empty
    val pTokens = if (pTA.hasView(ViewNames.SHALLOW_PARSE)) pTA.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala else Seq.empty

    def getParagraphConsCovering(c: Constituent): Option[Constituent] = {
      p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituentsCovering(c).asScala.headOption
    }

    ilpSolver.setAsMaximization()

    // whether to create the model with the tokenized version of the answer options
    val tokenizeAnswers = true //if(q.isTemporal) true else false
    val aTokens = if (tokenizeAnswers) {
      q.answers.map(_.aTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.map(_.getSurfaceForm))
    }
    else {
      q.answers.map(a => Seq(a.answerText))
    }

    def getAnswerOptionCons(ansIdx: Int, ansTokIdx: Int): Constituent = {
      q.answers(ansIdx).aTAOpt.get.getView(ViewNames.SHALLOW_PARSE).getConstituents.get(ansTokIdx)
    }

    // create questionToken-paragraphToken alignment edges
    val questionParagraphAlignments = for {
      qCons <- qTokens
      pCons <- pTokens
      // TODO: make it QuestionCell score
//      score = alignmentFunction.scoreCellCell(qCons.getSurfaceForm, pCons.getSurfaceForm) + params.questionCellOffset
      score = alignmentFunction.scoreCellQCons(pCons.getSurfaceForm, qCons.getSurfaceForm) + params.questionCellOffset
      if score > params.minParagraphToQuestionAlignmentScore
      x = ilpSolver.createBinaryVar("", score)
    } yield (qCons, pCons, x)

    // create paragraphToken-answerOption alignment edges
    val paragraphAnswerAlignments = if (!isTrueFalseQuestion) {
      // create only multiple nodes at each answer option
      for {
        pCons <- pTokens
        ansIdx <- aTokens.indices
        ansConsIdx <- aTokens(ansIdx).indices
        ansConsString = aTokens(ansIdx).apply(ansConsIdx)
        // TODO: make it QuestionCell score
        score = alignmentFunction.scoreCellCell(pCons.getSurfaceForm, ansConsString) + params.paragraphAnswerOffset
//        score = alignmentFunction.scoreCellQChoice(pCons.getSurfaceForm, ansConsString) + params.paragraphAnswerOffset
        x = ilpSolver.createBinaryVar("", score)
      } yield (pCons, ansIdx, ansConsIdx, x)
    } else {
      List.empty
    }

    // high-level variables
    // active answer options
    val activeAnswerOptions = if (!isTrueFalseQuestion) {
      for {
        ansIdx <- q.answers.indices
        x = ilpSolver.createBinaryVar("activeAnsOptId" + ansIdx, 0.0)
      } yield (ansIdx, x)
    }
    else {
      List.empty
    }

    // active answer option token
    /*val activeAnsweOptionToken = if(!isTrueFalseQuestion) {
      for {
        ansIdx <- aTokens.indices
        ansTokIdx <- aTokens(ansIdx).indices
        x = ilpSolver.createBinaryVar(s"activeAns${ansIdx}Tok${ansTokIdx}OptId", 0.0)
      } yield (ansIdx, ansTokIdx, x)
    }
    else {
      List.empty
    }
*/
    def getVariablesConnectedToOptionToken(ansIdx: Int, tokenIdx: Int): Seq[V] = {
      paragraphAnswerAlignments.filter { case (_, ansIdxTmp, tokenIdxTmp, _) =>
        ansIdxTmp == ansIdx && tokenIdxTmp == tokenIdx
      }.map(_._4)
    }

    def getVariablesConnectedToOption(ansIdx: Int): Seq[V] = {
      paragraphAnswerAlignments.filter { case (_, ansTmp, _, _) => ansTmp == ansIdx }.map(_._4)
    }

    def getAnswerOptionVariablesConnectedToParagraph(c: Constituent): Seq[(Int, Int, V)] = {
      paragraphAnswerAlignments.filter { case (cTmp, ansIdxTmp, tokenIdxTmp, _) => cTmp == c }.map(tuple => (tuple._2, tuple._3, tuple._4))
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

    /*
        // Answer option token must be active if anything connected to it is active
        activeAnsweOptionToken.foreach {
          case (ansIdx, ansTokIdx, x) =>
            val connectedVariables = getVariablesConnectedToOptionToken(ansIdx, ansTokIdx)
            val allVars = connectedVariables :+ x
            val coeffs = Seq.fill(connectedVariables.length)(-1.0) :+ 1.0
            ilpSolver.addConsBasicLinear("activeOptionTokVarImplesOneActiveConnectedEdge", allVars, coeffs, None, Some(0.0))
            connectedVariables.foreach { connectedVar =>
              val vars = Seq(connectedVar, x)
              val coeffs = Seq(1.0, -1.0)
              ilpSolver.addConsBasicLinear("activeConnectedEdgeImpliesOneAnswerOptionTok", vars, coeffs, None, Some(0.0))
            }
        }
    */

    // active paragraph constituent
    val activeParagraphConstituents = pTokens.map { t =>
      t -> ilpSolver.createBinaryVar("", params.activeParagraphConstituentsWeight)
    }.toMap
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
      // alignment is preferred for lesser sentences; hence: negative activeSentenceDiscount
        x = ilpSolver.createBinaryVar("activeSentence:" + s, params.activeSentencesDiscount)
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
      weight = if(SolverUtils.scienceTermsMap.contains(t.getSurfaceForm.toLowerCase)) {
        params.activeQuestionTermWeight + params.scieneTermBoost
      } else {
        params.activeQuestionTermWeight
      }
      x = ilpSolver.createBinaryVar("activeQuestionCons", weight)
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

    // weight for answers being close to each other
    /*val weight = -1.0
    aTokens.indices.map{ answerOptionIdx =>
      // for each answer option create one of these weights
      //val x = ilpSolver.createIntegerVar("answerTokenDistanceWeights", 0, 100, 1.0)
      aTokens.foreach{ toks =>
        val x = ilpSolver.createIntegerVar("answerTokenDistanceWeights", 0, 100, 1.0)
      }
      val len = aTokens(answerOptionIdx).length - 1
      for{
        i <- 0 until len - 1
        j <- i + 1 until len
      } {
        val x = ilpSolver.createIntegerVar(s"Answer:$answerOptionIdx-iAndjAnswerOption$i-$j", 0, 100, weight)
        // add constraint

      }

      (answerOptionIdx, x)
    }*/

    // extra weight for alignment of paragraph constituents
    // create edges between constituents which have an edge in the dependency parse
    // this edge can be active only if the base nodes are active
    def twoAnswerConsAreConnectedViaDependencyParse(ansIdx: Int, tokIdx1: Int, tokIdx2: Int): Boolean = {
//      println(s"ansIdx: $ansIdx, tokIdx1: $tokIdx1, tokIdx2: $tokIdx2")
      val cons1 = getAnswerOptionCons(ansIdx, tokIdx1)
      val cons2 = getAnswerOptionCons(ansIdx, tokIdx2)
//      println(s"cons1: $cons1 / cons2: $cons2")
      val ansDepView = q.answers(ansIdx).aTAOpt.get.getView(ViewNames.DEPENDENCY_STANFORD)
      val cons1InDep = ansDepView.getConstituentsCovering(cons1).asScala.headOption
      val cons2InDep = ansDepView.getConstituentsCovering(cons2).asScala.headOption
//      println(s"cons1InDep: $cons1InDep / cons2InDep: $cons2InDep")
      if (cons1InDep.isDefined && cons2InDep.isDefined) {
        val relations = ansDepView.getRelations.asScala
//        println("relations:" + relations)
        relations.exists { r =>
          (r.getSource == cons1InDep.get && r.getTarget == cons2InDep.get) ||
            (r.getSource == cons2InDep.get && r.getTarget == cons1InDep.get)
        }
      }
      else {
        false
      }
    }

    var edgeDependencyVariables: mutable.Buffer[(Constituent, Constituent, V)] = mutable.Buffer.empty
    if(p.contextTAOpt.get.hasView(ViewNames.DEPENDENCY_STANFORD)) {
      val depView = p.contextTAOpt.get.getView(ViewNames.DEPENDENCY_STANFORD)
      val depRelations = depView.getRelations.asScala
      edgeDependencyVariables = depRelations.zipWithIndex.map { case (r, idx) =>
        val startConsOpt = getParagraphConsCovering(r.getSource)
        val targetConsOpt = getParagraphConsCovering(r.getTarget)
        if (startConsOpt.isDefined && targetConsOpt.isDefined && startConsOpt.get != targetConsOpt.get) {
          val x = ilpSolver.createBinaryVar(s"Relation:$idx", params.firstOrderDependencyEdgeAlignments)

          // this relation variable is active, only if its two sides are active
          val startVar = activeParagraphConstituents(startConsOpt.get)
          val targetVar = activeParagraphConstituents(targetConsOpt.get)
          ilpSolver.addConsBasicLinear("dependencyVariableActiveOnlyIfSourceConsIsActive",
            Seq(x, startVar), Seq(1.0, -1.0), None, Some(0.0))
          ilpSolver.addConsBasicLinear("dependencyVariableActiveOnlyIfSourceConsIsActive",
            Seq(x, targetVar), Seq(1.0, -1.0), None, Some(0.0))

          val ansList1 = getAnswerOptionVariablesConnectedToParagraph(startConsOpt.get)
          val ansList2 = getAnswerOptionVariablesConnectedToParagraph(targetConsOpt.get)

          val variablesPairsInAnswerOptionsWithDependencyRelation = for {
            a <- ansList1
            b <- ansList2
            if a._1 == b._1 // same answer
            if a._2 != b._2 // different tok
            if twoAnswerConsAreConnectedViaDependencyParse(a._1, a._2, b._2) // they are connected via dep parse
          }
            yield {
              val weight = 0.0 // TODO: tune this
              val activePair = ilpSolver.createBinaryVar(s"activeAnsweOptionPairs", weight)
              ilpSolver.addConsBasicLinear("NoActivePairIfNonAreActive", Seq(a._3, b._3, activePair), Seq(-1.0, -1.0, 1.0), None, Some(0.0))
              ilpSolver.addConsBasicLinear("NoActivePairIfNonAreActive", Seq(activePair, a._3), Seq(-1.0, 1.0), None, Some(0.0))
              ilpSolver.addConsBasicLinear("NoActivePairIfNonAreActive", Seq(activePair, b._3), Seq(-1.0, 1.0), None, Some(0.0))
              activePair
            }
          //        println("ansList1: " + ansList1)
          //        println("ansList2: " + ansList2)
          //        println("variablesPairsInAnswerOptionsWithDependencyRelation: " + variablesPairsInAnswerOptionsWithDependencyRelation.length)

          // if the paragraph relation pair is active, at least one answer response pair should be active
          // in other words
          ilpSolver.addConsBasicLinear("atLeastOnePairShouldBeActive",
            variablesPairsInAnswerOptionsWithDependencyRelation :+ x,
            Array.fill(variablesPairsInAnswerOptionsWithDependencyRelation.length) {
              -1.0
            } :+ 1.0, None, Some(0.0))

          Some(startConsOpt.get, targetConsOpt.get, x)
        }
        else {
          None
        }
      }.collect { case a if a.isDefined => a.get }
    }
    else {
      println("Paragraph does not contain parse-view . . . ")
    }

    // constraints
    // alignment to only one option, i.e. there must be only one single active option
    if (activeAnswerOptions.nonEmpty) {
      val activeAnsVars = activeAnswerOptions.map { case (ans, x) => x }
      val activeAnsVarsCoeffs = Seq.fill(activeAnsVars.length)(1.0)
      ilpSolver.addConsBasicLinear("onlyOneActiveOption", activeAnsVars, activeAnsVarsCoeffs, Some(1.0), Some(1.0))
    }

    // for each of the answer options create one variable, turning on when the number of the alignments to that answer
    // option is more than k
/*    (1 to 3).foreach{ k: Int =>
      activeAnswerOptions.foreach { case (ansIdx, _) =>
      val penalty = k match {
        case 1  => params.moreThan1AlignmentAnsPenalty
        case 2  => params.moreThan2AlignmentAnsPenalty
        case 3  => params.moreThan3AlignmentAnsPenalty
      }
        val moreThanKAlignnetbToAnswerOption = ilpSolver.createBinaryVar(s"moreThan${k}AlignmentAnsPenalty", penalty)

        // this gets activated, if the answer option has at least two active alignments
        val connectedVariables = getVariablesConnectedToOption(ansIdx).toList
        val len = connectedVariables.length
        ilpSolver.addConsBasicLinear("",
          moreThanKAlignnetbToAnswerOption +: connectedVariables, (-len + k.toDouble) +: Array.fill(len) {1.0}, None, Some(k))
      }
    }*/


/*    // for each of the question terms create one variable, turning on when the number of the alignments to the
    // constituent is more than k
    for{ k: Double <- 1.0 to 3.0 } {
      qTokens.foreach { c =>
        val penalty = k match {
          case 1.0  => params.moreThan1AlignmentToQuestionTermPenalty
          case 2.0  => params.moreThan2AlignmentToQuestionTermPenalty
          case 3.0  => params.moreThan3AlignmentToQuestionTermPenalty
        }
        val moreThanKAlignnetbToQuestionCons = ilpSolver.createBinaryVar(
          s"moreThan${k}AlignmentToQuestionConsPenalty", penalty
        )

        // this gets activated, if the answer option has at least two active alignments
        val connectedVariables = getVariablesConnectedToQuestionToken(c).toList
        val len = connectedVariables.length
        ilpSolver.addConsBasicLinear("",
          moreThanKAlignnetbToQuestionCons +: connectedVariables, (-len + k) +: Array.fill(len) {
            1.0
          }, None, Some(k))
      }
    }*/

    // there is an upper-bound on the max number of active tokens in each sentence
    activeSentences.foreach { case (ans, x) =>
        val connectedVariables = getVariablesConnectedToParagraphSentence(ans)
        ilpSolver.addConsBasicLinear("activeParagraphConsVar",
          connectedVariables, Array.fill(connectedVariables.length){1.0},
          None, Some(params.maxNumberOfWordsAlignedPerSentence))
    }

    // among the words that are repeated in the paragraph, at most k of them can be active
    // first find the duplicate elements
    val duplicates = pTokens.groupBy(_.getSurfaceForm).filter { case (x,ys) => ys.lengthCompare(1) > 0 }
    duplicates.foreach{ case (_, duplicateCons) =>
      val variables = duplicateCons.map(activeParagraphConstituents)
      ilpSolver.addConsBasicLinear("", variables, Array.fill(variables.length){1.0},
        None, Some(params.maxAlignmentToRepeatedWordsInParagraph))
    }

    // have at most k active sentence
    val (_, sentenceVars) = activeSentences.unzip
    val sentenceVarsCoeffs = Seq.fill(sentenceVars.length)(1.0)
    ilpSolver.addConsBasicLinear("maxActiveParagraphConsVar", sentenceVars, sentenceVarsCoeffs,
      Some(0.0), Some(params.maxActiveSentences))


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


/*
    require(params.coreferenceWeight>=0, "params.coreferenceWeight should be positive")
    val corefCons = if (pTA.hasView(ViewNames.COREF)) pTA.getView(ViewNames.COREF).getConstituents.asScala else Seq.empty
    corefCons.groupBy(_.getLabel).foreach{ case (_, cons) =>  // cons that have the same label are co-refered
      cons.zipWithIndex.combinations(2).foreach{ consPair =>
        val x = ilpSolver.createBinaryVar(s"coredEdgeVariable${consPair.head._2}-${consPair(1)._2}", params.coreferenceWeight)
        val x1 = activeParagraphConstituents(consPair.head._1)
        val x2 = activeParagraphConstituents(consPair(1)._1)
        ilpSolver.addConsBasicLinear(s"coreEdgePairCons-${consPair.head._2}", Seq(x, x1), Seq(1.0, 1.0), None, Some(0.0))
        ilpSolver.addConsBasicLinear(s"coreEdgePairCons-${consPair(1)._2}", Seq(x, x2), Seq(1.0, 1.0), None, Some(0.0))
      }
    }
*/

/*    if(useSRL && q.qTAOpt.get.hasView(ViewNames.SRL_VERB) && p.contextTAOpt.get.hasView(ViewNames.SRL_VERB) ) {
      val qSRLView = q.qTAOpt.get.getView(ViewNames.SRL_VERB)
      val pSRLView = p.contextTAOpt.get.getView(ViewNames.SRL_VERB)
      val qSRLCons = qSRLView.getConstituents.asScala
      val pSRLCons = pSRLView.getConstituents.asScala

      // does question-srl contain any question term?
      val cons = qSRLCons.filter(c => CandidateGeneration.questionTerms.contains(c.getSurfaceForm.toLowerCase))
      if (verbose) println(">>>>>> cons: " + cons)
      if(cons.nonEmpty) {
        if(cons.size > 1) {
          println("More than two cons overlapping with the question term. Choosing the head . . . ")
        }
        if (verbose) println("question: " + q.questionText)
        val qArgLabel = cons.head.getLabel
        if (verbose) println(">>>>>>> qArgLabel: " + qArgLabel)
        val qSource = cons.head.getIncomingRelations.get(0).getSource
        if (verbose) println(">>>>>>>> " + qSource)
        def getLabel(pred: Constituent): String = pred.getLabel + pred.getAttribute("SenseNumber") + pred.getAttribute("predicate")
        val qSourceLabel = getLabel(qSource)
        if (verbose) println(">>>>>>>> " + qSourceLabel)
        val pParagraph = pSRLCons.filter(c => getLabel(c) == qSourceLabel)
        val pArgCons = pParagraph.flatMap{ pred =>
          pred.getOutgoingRelations.asScala.map(_.getTarget).filter(_.getLabel == qArgLabel)
        }
        if (verbose) println("pArgCons: " + pArgCons)
        // check if any of the any of the selected constituents overlap with one of the answers
        def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
          pCons.map { c =>
            TextILPSolver.getAvgScore(qCons, Seq(c))
          }.zipWithIndex.maxBy(_._1)._2
        }
        val pCons = p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
        val results = q.answers.map{ ans =>
          val ansCons = ans.aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
          val ansParagraphIdx = getClosestIndex(ansCons, pCons)
          val ansSpan = pCons(ansParagraphIdx).getSpan
          if (verbose) println("ansCons: " + ansCons)
          if (verbose) println("pCons(ansParagraphIdx): " + pCons(ansParagraphIdx))
          if(pArgCons.exists{ pArg => pArg.getSpan.getFirst <= ansSpan.getFirst &&  pArg.getSpan.getSecond >= ansSpan.getSecond }) 1 else 0
        }
        if (verbose) println("results: " + results)
        if(results.sum == 1) { // i.e. only one of the answer options has overlap
          results.zipWithIndex.foreach{ case (v, idx) =>
            if(v == 1) {
              println(s"SRL alignment: Setting the answer $idx to be active")
              ilpSolver.addConsBasicLinear("resultReasoning-secondAnswerMustBeCorrect", Seq(activeAnswerOptions(idx)._2), Seq(1.0), Some(1.0), None)
            }
          }
        }
      }
    }*/

    // longer than 1 answer penalty
    activeAnswerOptions.foreach{ case (ansIdx, activeAnsVar) =>
      val ansTokList = aTokens(ansIdx)
      if(ansTokList.length > 1) {
        val x = ilpSolver.createBinaryVar("longerThanOnePenalty", params.longerThan1TokenAnsPenalty)
        ilpSolver.addConsBasicLinear("longerThanOnePenaltyActiveOnlyWhenOptionIsActive",
          Seq(x, activeAnsVar), Seq(-1.0, 1.0), None, Some(0.0))
      }
      if(ansTokList.length > 2) {
        val x = ilpSolver.createBinaryVar("longerThanTwoPenalty", params.longerThan2TokenAnsPenalty)
        ilpSolver.addConsBasicLinear("longerThanOnePenaltyActiveOnlyWhenOptionIsActive",
          Seq(x, activeAnsVar), Seq(-1.0, 1.0), None, Some(0.0))
      }
      if(ansTokList.length > 3) {
        val x = ilpSolver.createBinaryVar("longerThanThreePenalty", params.longerThan3TokenAnsPenalty)
        ilpSolver.addConsBasicLinear("longerThanThreePenaltyActiveOnlyWhenOptionIsActive",
          Seq(x, activeAnsVar), Seq(-1.0, 1.0), None, Some(0.0))
      }
    }

    // use at least k constituents in the question
    val (_, questionVars) = activeQuestionConstituents.unzip
    val questionVarsCoeffs = Seq.fill(questionVars.length)(1.0)
    ilpSolver.addConsBasicLinear("activeQuestionConsVarNum", questionVars,
      questionVarsCoeffs, Some(params.minQuestionTermsAligned), Some(params.maxQuestionTermsAligned))
    ilpSolver.addConsBasicLinear("activeQuestionConsVarRatio", questionVars,
      questionVarsCoeffs,
      Some(params.minQuestionTermsAlignedRatio * questionVars.length),
      Some(params.maxQuestionTermsAlignedRatio * questionVars.length))

    // if anything comes after " without " it should be aligned definitely
    // example: What would happen without annealing?
    if (q.questionText.contains(" without ")) {
      if (verbose) println(" >>> Adding constraint to use the term after `without`")
      val withoutTok = qTokens.filter(_.getSurfaceForm == "without").head
      if (verbose) println("withoutTok: " + withoutTok)
      val after = qTokens.filter(c => c.getStartSpan > withoutTok.getStartSpan).minBy(_.getStartSpan)
      if (verbose) println("after: " + after)
      val afterVariableOpt = activeQuestionConstituents.collectFirst { case (c, v) if c == after => v }
      if (verbose) println("afterVariableOpt = " + afterVariableOpt)
      afterVariableOpt match {
        case Some(afterVariable) =>
          ilpSolver.addConsBasicLinear("termAfterWithoutMustBeAligned", Seq(afterVariable), Seq(1.0), Some(1.0), None)
        case None => // do nothing
      }
    }


    val interParagraphAlignments = edgeDependencyVariables

    if (verbose) println("created the ilp model. Now solving it  . . . ")

    val modelSolveStart = System.currentTimeMillis()

    // solving and extracting the answer
    ilpSolver.solve()

    val modelSolveEnd = System.currentTimeMillis()

    val statistics = Stats(numberOfBinaryVars = ilpSolver.getNBinVars,
      numberOfContinuousVars = ilpSolver.getNContVars, numberOfIntegerVars = ilpSolver.getNIntVars,
      numberOfConstraints = ilpSolver.getNConss, ilpIterations = ilpSolver.getNLPIterations,
      modelCreationInSec = (modelSolveEnd - modelSolveStart) / 1000.0, solveTimeInSec = (modelSolveStart - modelCreationStart) / 1000.0)
    if(verbose) {
      println("Statistics: " + statistics)
    }


    if (verbose) println("Done solving the model  . . . ")

    if (verbose) {
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
      seq.map { case (id, x) => "id: " + id + " : " + ilpSolver.getSolVal(x) }.mkString(" / ")
    }

    def stringifyVariableSequence3(seq: Seq[(Constituent, V)])(implicit d: DummyImplicit): String = {
      seq.map { case (id, x) => "id: " + id.getSurfaceForm + " : " + ilpSolver.getSolVal(x) }.mkString(" / ")
    }

    def stringifyVariableSequence2(seq: Seq[(Constituent, Constituent, V)])(implicit d: DummyImplicit, d2: DummyImplicit): String = {
      seq.map { case (c1, c2, x) => "c1: " + c1.getSurfaceForm + ", c2: " + c2.getSurfaceForm + " -> " + ilpSolver.getSolVal(x) }.mkString(" / ")
    }

    def stringifyVariableSequence4(seq: Seq[(Constituent, Int, Int, V)]): String = {
      seq.map { case (c, i, j, x) => "c: " + c.getSurfaceForm + ", ansIdx: " + i + ", ansConsIdx: " + j + " -> " + ilpSolver.getSolVal(x) }.mkString(" / ")
    }

    if (ilpSolver.getStatus == IlpStatusOptimal) {
      if (verbose) println("Primal score: " + ilpSolver.getPrimalbound)
      val trueIdx = q.trueIndex
      val falseIdx = q.falseIndex
      val selectedIndex = if (isTrueFalseQuestion) {
        if (ilpSolver.getPrimalbound > TextILPSolver.trueFalseThreshold) Seq(trueIdx) else Seq(falseIdx)
      }
      else {
        if (verbose) println(">>>>>>> not true/false . .. ")
        activeAnswerOptions.zipWithIndex.collect { case ((ans, x), idx) if ilpSolver.getSolVal(x) > 1.0 - TextILPSolver.epsilon => idx }
      }
      val questionBeginning = "Question: "
      val paragraphBeginning = "|Paragraph: "
      val questionString = questionBeginning + q.questionText
      val choiceString = "|Options: " + q.answers.zipWithIndex.map { case (ans, key) => s" (${key + 1}) " + ans.answerText }.mkString(" ")
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
            val t1 = if (!entityMap.contains(span1)) {
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
            val t2 = if (!entityMap.contains(span2)) {
              val t2 = "T" + eIter
              eIter = eIter + 1
              entities += Entity(t2, c2.getSurfaceForm, Seq(span2))
              entityMap.put(span2, t2)
              t2
            }
            else {
              entityMap(span2)
            }

            if (!relationSet.contains((t1, t2))) {
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
            val t1 = if (!entityMap.contains(span1)) {
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
            val t2 = if (!entityMap.contains(span2)) {
              val t2 = "T" + eIter
              entities += Entity(t2, ansString, Seq(span2))
              eIter = eIter + 1
              entityMap.put(span2, t2)
              t2
            }
            else {
              entityMap(span2)
            }

            if (!relationSet.contains((t1, t2))) {
              relations += Relation("R" + rIter, t1, t2, ilpSolver.getVarObjCoeff(x))
              rIter = rIter + 1
              relationSet.add((t1, t2))
            }
          }
      }

      // inter-paragraph alignments
      interParagraphAlignments.foreach { case (c1, c2, x) =>
        if (ilpSolver.getSolVal(x) > 1.0 - TextILPSolver.epsilon) {
          val pBeginIndex1 = c1.getStartCharOffset + questionString.length + paragraphBeginning.length
          val pEndIndex1 = pBeginIndex1 + c1.getSurfaceForm.length
          val span1 = (pBeginIndex1, pEndIndex1)
          val t1 = if (!entityMap.contains(span1)) {
            val t1 = "T" + eIter
            entities += Entity(t1, c1.getSurfaceForm, Seq(span1))
            entityMap.put(span1, t1)
            eIter = eIter + 1
            t1
          } else {
            entityMap(span1)
          }
          val pBeginIndex2 = c2.getStartCharOffset + questionString.length + paragraphBeginning.length
          val pEndIndex2 = pBeginIndex2 + c2.getSurfaceForm.length
          val span2 = (pBeginIndex2, pEndIndex2)
          val t2 = if (!entityMap.contains(span2)) {
            val t2 = "T" + eIter
            entities += Entity(t2, c1.getSurfaceForm, Seq(span2))
            entityMap.put(span2, t2)
            eIter = eIter + 1
            t2
          } else {
            entityMap(span2)
          }

          if (!relationSet.contains((t1, t2))) {
            relations += Relation("R" + rIter, t1, t2, ilpSolver.getVarObjCoeff(x))
            rIter = rIter + 1
            relationSet.add((t1, t2))
          }
        }

        if (isTrueFalseQuestion) {
          // add the answer option span manually
          selectedIndex.foreach { idx =>
            val ansText = q.answers(idx).answerText
            val oBeginIndex = choiceString.indexOf(ansText) + questionString.length + paragraphString.length
            val oEndIndex = oBeginIndex + ansText.length
            val span2 = (oBeginIndex, oEndIndex)
            val t2 = if (!entityMap.contains(span2)) {
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
      }

      if (verbose) println("returning the answer  . . . ")

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
      if (verbose) println("Not optimal . . . ")
      if (verbose) println("Status is not optimal. Status: " + ilpSolver.getStatus)
      // if the program is not solver, say IDK
      Seq.empty -> EntityRelationResult("INFEASIBLE", List.empty, List.empty, statistics = statistics)
    }
  }

}
