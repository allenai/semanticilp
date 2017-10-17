package org.allenai.ari.solvers.textilp

import edu.illinois.cs.cogcomp.McTest.MCTestBaseline
import edu.illinois.cs.cogcomp.core.utilities.configuration.ResourceManager
import org.allenai.ari.solvers.bioProccess.ProcessBankReader
import org.allenai.ari.solvers.textilp.solvers.{ CauseRule, WhatDoesItDoRule, _ }
import org.allenai.ari.solvers.textilp.alignment.AlignmentFunction
import org.allenai.ari.solvers.textilp.utils._
import org.rogach.scallop._
import ProcessBankReader._
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, TextAnnotation }
import edu.illinois.cs.cogcomp.core.io.IOUtils
import edu.illinois.cs.cogcomp.edison.annotators.ClauseViewGenerator
import edu.illinois.cs.cogcomp.pipeline.server.ServerClientAnnotator
import org.simmetrics.metrics.StringMetrics
import org.allenai.ari.controller.questionparser.{ FillInTheBlankGenerator, QuestionParse }
import org.apache.commons.io.FilenameUtils
import org.simmetrics.StringMetric
import weka.classifiers.Evaluation
import weka.classifiers.bayes.NaiveBayesUpdateable
import weka.classifiers.functions.Logistic
import weka.core.Instances
import weka.core.converters.ArffLoader
import weka.core.converters.ArffLoader.ArffReader
import java.io._
import java.net.URL
import java.util

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import play.api.libs.json.{ JsArray, JsObject, Json }

import scala.util.Random
import scala.io.Source
import scala.collection.JavaConverters._

object ExperimentsApp {
  lazy val annotationUtils = new AnnotationUtils()
  lazy val textILPSolver = new TextILPSolver(annotationUtils, verbose = false, SolverUtils.params)
  lazy val salienceSolver = new SalienceSolver()
  lazy val luceneSolver = new LuceneSolver()
  lazy val slidingWindowSolver = new SlidingWindowSolver()

  class ArgumentParser(args: Array[String]) extends ScallopConf(args) {
    val experimentType: ScallopOption[Int] = opt[Int]("type", descr = "Experiment type", required = true)
    verify()
  }

  def evaluateTextSolverOnRegents(dataset: Seq[(String, Seq[String], String)], textSolver: TextSolver,
    knowledgeLength: Int = 8, printMistakes: Boolean = false, splitToSentences: Boolean = false) = {
    val resultFile = new PrintWriter(new File(s"output/results-$textSolver-length${dataset.length}.txt"))
    val types = Seq(WhatDoesItDoRule, CauseRule, SRLV1Rule, VerbSRLandPrepSRL, SRLV1ILP, VerbSRLandCoref, SimpleMatching)
    import java.io._
    // use false if you don't it to write things on disk
    val predictionsFileOpt = if (true) {
      Some(new PrintWriter(new File(s"output-$textSolver-length${dataset.length}.tsv")))
    } else {
      None
    }
    val start = System.currentTimeMillis()
    SolverUtils.printMemoryDetails()
    val max = dataset.length
    val (perQuestionScore, perQuestionResults, otherTimes) = dataset.zipWithIndex.map {
      case ((question, options, correct), idx) =>
        println(s"Processing $idx out of $max")
        val knowledgeStart = System.currentTimeMillis()
        val knowledgeSnippet = if (textSolver.isInstanceOf[TextILPSolver]) {
          val rawSentences = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, knowledgeLength).mkString(". ")
          annotationUtils.dropRedundantSentences(rawSentences)
        } else {
          ""
        }

        println("question: " + question)
        println("knowledge: " + knowledgeSnippet)
        val knowledgeEnd = System.currentTimeMillis()
        var bestSelected: Seq[Int] = Seq.empty
        val (selected, results) = if (knowledgeSnippet.trim != "") {
          println("solving it . . . ")
          if (splitToSentences) {
            val ta = annotationUtils.pipelineServerClient.annotate(knowledgeSnippet)
            val output = types.find { t =>
              println("--> method: " + t)
              val out = ta.getView(ViewNames.SENTENCE).getConstituents.asScala.find { c =>
                println("     ---> sentence: " + c.getSurfaceForm)
                val (selected, _) = textSolver.asInstanceOf[TextILPSolver].solveWithReasoningType(question, options, knowledgeSnippet, t)
                bestSelected = selected
                println("     ---> selected: " + selected)
                selected.nonEmpty
              }
              out.isDefined
            }
            bestSelected -> EntityRelationResult()
          } else {
            textSolver.solve(question, options, knowledgeSnippet)
          }
        } else {
          Seq.empty -> EntityRelationResult()
        }
        if (knowledgeSnippet.trim.isEmpty && textSolver.isInstanceOf[TextILPSolver]) {
          println("Error: knowledge not found .  .  .")
        }

        val solveEnd = System.currentTimeMillis()
        val score = SolverUtils.assignCredit(selected, correct.head - 'A', options.length)
        if (predictionsFileOpt.isDefined) predictionsFileOpt.get.write(question + "\t" + score + "\t" + selected + "\t" +
          results.statistics.numberOfBinaryVars + "\t" + results.statistics.numberOfConstraints + "\t" + results.statistics.objectiveValue + "\n")
        if (printMistakes && score < 1.0) {
          println("Question: " + question + " / options: " + options + "   / selected: " + selected + " / score: " + score)
        }
        println("Score " + score + "  selected: " + selected)
        (score, results.statistics,
          ((knowledgeEnd - knowledgeStart) / 1000.0, (solveEnd - knowledgeEnd) / 1000.0, if (selected.nonEmpty) 1.0 else 0.0))
    }.unzip3
    val (avgKnowledgeTime, avgSolveTime, nonEmptyList) = otherTimes.unzip3
    val avgCoverage = nonEmptyList.sum / nonEmptyList.length
    val nonZeroScores = perQuestionScore.zip(nonEmptyList).filter(_._2 > 0.0).map(_._1)
    println("Average score: " + perQuestionScore.sum / perQuestionScore.size)
    println("Avg precision: " + nonZeroScores.sum / nonZeroScores.size)
    println("avgCoverage: " + avgCoverage)
    println("Total number of questions: " + perQuestionResults.length)
    println("Total number of questions: " + nonZeroScores.length)
    println("Avg solve overall time: " + avgSolveTime.sum / avgSolveTime.length)
    println("Avg knowledge extraction time: " + avgKnowledgeTime.sum / avgKnowledgeTime.length)
    val end = System.currentTimeMillis()
    println("Total time (mins): " + (end - start) / 60000.0)
    resultFile.write(s"score: ${perQuestionScore.sum / perQuestionScore.size} " +
      s"\nPrecision: ${nonZeroScores.sum / nonZeroScores.size} " +
      s"\nCoverage: ${avgCoverage} " +
      s"\nAvgOverallTime: ${avgSolveTime.sum / avgSolveTime.length}")
    val avgResults = perQuestionResults.reduceRight[Stats] { case (a: Stats, b: Stats) => a.sumWith(b) }.divideBy(perQuestionResults.length)
    println(avgResults.toString)
    if (predictionsFileOpt.isDefined) predictionsFileOpt.get.close
    resultFile.close()
  }

  def evaluateTextSolverOnRegentsPerReasoningMethod(dataset: Seq[(String, Seq[String], String)], textSolver: TextSolver,
    knowledgeLength: Int = 8, printMistakes: Boolean = false) = {
    import java.io._
    val f = new File(s"output/results-per-solver-length2${dataset.length}.txt")
    val resultFile = new FileWriter(f)
    resultFile.write(s"Type \t SRL \t Score \t Precision \t Recall \t Total Answered \t Out of \t Total Time \n")
    resultFile.close()
    val types = Seq( /*SimpleMatching, SRLV1Rule, */ VerbSRLandPrepSRL, SRLV1ILP, VerbSRLandCoref, SRLV2Rule, SRLV3Rule, VerbSRLandCommaSRL)
    val srlViewsAll = Seq(ViewNames.SRL_VERB, TextILPSolver.curatorSRLViewName, TextILPSolver.pathLSTMViewName)
    types.foreach { t =>
      val start = System.currentTimeMillis()
      SolverUtils.printMemoryDetails()
      val srlViewTypes = if (t == CauseRule || t == WhatDoesItDoRule || t == SimpleMatching) Seq(TextILPSolver.pathLSTMViewName) else srlViewsAll
      srlViewTypes.foreach { srlVu =>
        // use false if you don't it to write things on disk
        val max = dataset.length
        val (perQuestionScore, perQuestionResults, otherTimes) = dataset.zipWithIndex.map {
          case ((question, options, correct), idx) =>
            println(s"Processing $idx out of $max")
            val knowledgeStart = System.currentTimeMillis()
            val knowledgeSnippet = if (textSolver.isInstanceOf[TextILPSolver]) {
              val rawSentences = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, knowledgeLength).mkString(". ")
              annotationUtils.dropRedundantSentences(rawSentences)
            } else {
              ""
            }

            println("knowledge: " + knowledgeSnippet)
            val knowledgeEnd = System.currentTimeMillis()

            val (selected, results) = if (knowledgeSnippet.trim != "") {
              println("solving it . . . ")
              textSolver.asInstanceOf[TextILPSolver].solveWithReasoningType(question, options, knowledgeSnippet, t, srlVu)
            } else {
              Seq.empty -> EntityRelationResult()
            }
            if (knowledgeSnippet.trim.isEmpty && textSolver.isInstanceOf[TextILPSolver]) {
              println("Error: knowledge not found .  .  .")
            }

            val solveEnd = System.currentTimeMillis()
            val score = SolverUtils.assignCredit(selected, correct.head - 'A', options.length)
            println(results.statistics)
            if (printMistakes && score < 1.0) {
              println("Question: " + question + " / options: " + options + "   / selected: " + selected + " / score: " + score)
            }
            println("Score " + score + "  selected: " + selected)
            (score, results.statistics,
              ((knowledgeEnd - knowledgeStart) / 1000.0, (solveEnd - knowledgeEnd) / 1000.0, if (selected.nonEmpty) 1.0 else 0.0))
        }.unzip3
        val (avgKnowledgeTime, avgSolveTime, nonEmptyList) = otherTimes.unzip3
        val avgCoverage = nonEmptyList.sum / nonEmptyList.length
        val nonZeroScores = perQuestionScore.zip(nonEmptyList).filter(_._2 > 0.0).map(_._1)
        println("reasoning type:  " + t)
        println("Average score: " + perQuestionScore.sum / perQuestionScore.size)
        println("Avg precision: " + nonZeroScores.sum / nonZeroScores.size)
        println("avgCoverage: " + avgCoverage)
        println("Total number of questions: " + perQuestionResults.length)
        println("Total number of questions: " + nonZeroScores.length)
        println("Avg solve overall time: " + avgSolveTime.sum / avgSolveTime.length)
        println("Avg knowledge extraction time: " + avgKnowledgeTime.sum / avgKnowledgeTime.length)
        val end = System.currentTimeMillis()
        println("Total time (mins): " + (end - start) / 60000.0)
        val avgResults = perQuestionResults.reduceRight[Stats] { case (a: Stats, b: Stats) => a.sumWith(b) }.divideBy(perQuestionResults.length)
        println(avgResults.toString)
        val resultFile = new FileWriter(f, true)
        resultFile.write(s"${t} \t  ${srlVu} \t ${perQuestionScore.sum / perQuestionScore.size} \t ${nonZeroScores.sum / nonZeroScores.size} \t ${avgCoverage} " +
          s"\t ${nonZeroScores.length} \t ${perQuestionResults.length} \t ${avgSolveTime.sum / avgSolveTime.length} \n")
        resultFile.close()
      }
    }
  }

  def cacheTheKnowledgeOnDisk(dataset: Seq[(String, Seq[String], String)]): Unit = {
    dataset.zipWithIndex.foreach {
      case ((question, options, correct), idx) =>
        println(s"done with $idx out of ${dataset.length}. ")
        options.zipWithIndex.foreach {
          case (f, opt) =>
            println("\t\t----> opt: " + opt)
            SolverUtils.staticCacheLucene(question, f, 200)
        }
    }
  }

  def evaluateTextSolverOnProcessBank(list: List[Paragraph], textSolver: TextSolver) = {
    import java.io._
    // use false if you don't it to write things on disk
    val outputFileOpt = if (true) {
      Some(new PrintWriter(new File("output.tsv")))
    } else {
      None
    }
    val qAndpPairs = list.flatMap { p => p.questions.map(q => (q, p)) }
    val resultFile = new PrintWriter(new File(s"output/results-$textSolver-length${qAndpPairs.length}.txt"))
    val (resultLists, stats, nonEmptyList) = qAndpPairs.zipWithIndex.map {
      case ((q, p), idx) =>
        println("Processed " + idx + " out of " + qAndpPairs.size)
        val candidates = q.answers.map(_.answerText)
        val correctIndex = q.correctIdxOpt.get
        val (selected, explanation) = textSolver.solve(q.questionText, candidates, p.context)
        val correctLabel = q.answers(correctIndex).answerText
        val score = SolverUtils.assignCredit(selected, correctIndex, candidates.length)
        if (outputFileOpt.isDefined) outputFileOpt.get.write(q.questionText + "\t" + score + "\t" + selected + "\n")
        (score, explanation.statistics, if (selected.nonEmpty) 1.0 else 0.0)
    }.unzip3
    val avgStats = stats.reduceRight[Stats] { case (a: Stats, b: Stats) => a.sumWith(b) }.divideBy(stats.length)
    val avgCoverage = nonEmptyList.sum / nonEmptyList.length
    val nonEmptyScores = resultLists.zip(nonEmptyList).filter(_._2 > 0).map(_._1)
    val avgPrecision = nonEmptyScores.sum / nonEmptyScores.length
    val avgAristoScore = resultLists.sum / resultLists.length
    println("------------")
    println("avgAristoScore: " + avgAristoScore)
    println("avgPrecision: " + avgPrecision)
    println("avgCoverage: " + avgCoverage)
    println("total size: " + resultLists.length)
    println("total answered: " + nonEmptyScores.length)
    resultFile.write("avgAristoScore: " + avgAristoScore + "\navgPrecision: " +
      avgPrecision + "\navgCoverage: " + avgCoverage + "\nresultLists.length: " + resultLists.length)
    if (outputFileOpt.isDefined) outputFileOpt.get.close
    resultFile.close()
  }

  def evaluateTextSolverOnProcessBankWithDifferentReasonings(list: List[Paragraph], textILPSolver: TextILPSolver) = {
    import java.io._
    //val types = Seq( /*WhatDoesItDoRule, CauseRule, */ SRLV1Rule /*, VerbSRLandPrepSRL,SRLV1ILP, VerbSRLandCoref, SimpleMatching*/ )
    val types = Seq(SimpleMatching /*SimpleMatching, WhatDoesItDoRule, CauseRule, SRLV1Rule, VerbSRLandPrepSRL, */ /*SRLV1ILP, VerbSRLandCoref,
                SRLV2Rule, SRLV3Rule*/ /*VerbSRLandCommaSRL*/ )
    val srlViewsAll = Seq(ViewNames.SRL_VERB, TextILPSolver.curatorSRLViewName, TextILPSolver.pathLSTMViewName)
    val qAndpPairs = list.flatMap { p => p.questions.map(q => (q, p)) }
    val resultFile = new PrintWriter(new File(s"output/results-per-solver-length${qAndpPairs.length}-processBank.txt"))
    resultFile.write(s"Type \t SRL \t Score \t Precision \t Recall \t Total Answered \t Out of \n")
    types.foreach { t =>
      println("==================================================")
      val srlViewTypes = if (t == CauseRule || t == WhatDoesItDoRule || t == SimpleMatching) Seq(TextILPSolver.pathLSTMViewName) else srlViewsAll
      // use false if you don't it to write things on disk
      srlViewTypes.foreach { srlVu =>
        val outputFileOpt = if (true) {
          Some(new PrintWriter(new File(s"output/${list.length}-${t.toString}-$srlVu.tsv")))
        } else {
          None
        }
        val (resultLists, stats, nonEmptyList) = qAndpPairs.zipWithIndex.collect {
          case ((q, p), idx) =>
            //println("==================================================")
            println("Processed " + idx + " out of " + qAndpPairs.size)
            //println("Paragraph: " + p)
            val candidates = q.answers.map(_.answerText)
            val correctIndex = q.correctIdxOpt.get
            //println("question: " + q.questionText)
            //println("candidates: " + candidates)
            //          println("length of allCandidatesMinusCorrectOnes: " + allCandidatesMinusCorrectOnes.size)
            //          println("candidates.length: " + candidates.length)
            val (selected, explanation) = textILPSolver.solveWithReasoningType(q.questionText, candidates, p.context, t, srlVu)
            val correctLabel = q.answers(correctIndex).answerText
            val score = SolverUtils.assignCredit(selected, correctIndex, candidates.length)
            //println("correctIndex: " + correctIndex)
            if (outputFileOpt.isDefined) outputFileOpt.get.write(q.questionText + "\t" + score + "\t" + correctIndex +
              "\t" + selected + "\t" + explanation.statistics.numberOfBinaryVars + "\t" + explanation.statistics.numberOfConstraints +
              "\t" + explanation.statistics.objectiveValue + "\n")

            //println("selected: " + selected + " score: " + score + " explanation: " + explanation)
            //          if (score < 0.5 && selected.nonEmpty) println(" >>>>>>> Selected and Incorrect :" + score + s"  ${q.questionText}")
            //          if (score < 0.5) println(" >>>>>>> Incorrect :" + score)
            //          if (score > 0.5) println(" >>>>>>> correct :" + score)
            //          println(s"Processed $idx out of ${qAndpPairs.length}")
            (score, explanation.statistics, if (selected.nonEmpty) 1.0 else 0.0) // -> (explanation.confidence -> correctLabel)
        }.unzip3

        val avgStats = stats.reduceRight[Stats] { case (a: Stats, b: Stats) => a.sumWith(b) }.divideBy(stats.length)
        val avgCoverage = nonEmptyList.sum / nonEmptyList.length
        val nonEmptyScores = resultLists.zip(nonEmptyList).filter(_._2 > 0).map(_._1)
        val avgPrecision = nonEmptyScores.sum / nonEmptyScores.length
        val avgAristoScore = resultLists.sum / resultLists.length
        println("------------")
        println("t: " + t.toString)
        println("avgAristoScore: " + avgAristoScore)
        println("avgPrecision: " + avgPrecision)
        println("avgCoverage: " + avgCoverage)
        println("total size: " + resultLists.length)
        println("total answered: " + nonEmptyScores.length)
        if (outputFileOpt.isDefined) outputFileOpt.get.close
        resultFile.write(s"${t} \t  ${srlVu} \t $avgAristoScore \t $avgPrecision \t ${avgCoverage} " +
          s"\t ${nonEmptyScores.length} \t ${resultLists.length} \n")
      }
    }
    resultFile.close()
  }

  def main(args: Array[String]): Unit = {
    lazy val processReader = new ProcessBankReader(false, annotationUtils)
    val parser = new ArgumentParser(args)
    parser.experimentType() match {
      case 1 =>
        val startTime = System.currentTimeMillis()
        val (selected, statistics) = textILPSolver.solve(
          "A decomposer is an organism that",
          Seq("hunts and eats animals", "migrates for the winter",
            "breaks down dead plants and animals", "uses water and sunlight to make food"),
          "organisms that obtain energy by eating dead plant or animal matter. " +
            "DECOMPOSER An organism that breaks down cells of dead plants and animals into simpler substances." +
            "The plants use sunlight, carbon dioxide, water, and minerals to make food that sustains themselves and other organisms in the forest."
        )
        println(selected)
        println(statistics)
        val endTime = System.currentTimeMillis()
        println("total time: " + (endTime - startTime) / 1000.0)
      case 2 =>
        // evaluateTextSolverOnRegents(SolverUtils.eigthGradeTrain, textILPSolver, splitToSentences = true)
        // evaluateTextSolverOnRegents(SolverUtils.eigthGradeTest, textILPSolver, splitToSentences = true)

        // evaluateTextSolverOnRegents(SolverUtils.regentsTrain, textILPSolver, splitToSentences = true)
        // println("==== regents train / sentence split = true  ")
        // evaluateTextSolverOnRegents(SolverUtils.regentsTest, textILPSolver, splitToSentences = true)
        // println("==== regents test  / sentence split = true  ")
        evaluateTextSolverOnRegents(SolverUtils.regentsTrain, textILPSolver, splitToSentences = false)
        println("==== regents train  ")
        evaluateTextSolverOnRegents(SolverUtils.regentsTest, textILPSolver, splitToSentences = false)
        println("==== regents test  ")

      // evaluateTextSolverOnRegents(SolverUtils.regentsPerturbed, textILPSolver)
      // println("==== regents perturbed  ")
      // evaluateTextSolverOnRegents(SolverUtils.publicTrain, textILPSolver)
      // println("==== public train ")
      // evaluateTextSolverOnRegents(SolverUtils.publicDev, textILPSolver)
      // println("==== public dev ")
      // evaluateTextSolverOnRegents(SolverUtils.publicTest, textILPSolver)
      // println("==== public test ")
      // evaluateTextSolverOnRegents(SolverUtils.omnibusTrain, textILPSolver)
      // println("==== omnibus train ")
      // evaluateTextSolverOnRegents(SolverUtils.omnibusTest, textILPSolver)
      // println("==== omnibus test ")
      // evaluateTextSolverOnRegents(SolverUtils.regentsPerturbed, luceneSolver)
      // println("==== regents perturbed  ")
      // evaluateTextSolverOnRegents(SolverUtils.omnibusTest, luceneSolver)
      // println("==== omnibus test  ")
      // evaluateTextSolverOnRegents(SolverUtils.regentsTest, luceneSolver)
      // println("==== regents test  ")
      // evaluateTextSolverOnRegents(SolverUtils.publicTest, luceneSolver)
      // println("==== public test  ")
      case 3 =>
        // get dataset statistics
        val allParagraphs = processReader.testInstances ++ processReader.trainingInstances
        println("paragraphs: " + allParagraphs.length)
        println("number of questions: " + allParagraphs.flatMap(_.questions).length)
        println("number of training questions: " + processReader.trainingInstances.flatMap(_.questions).length)
        println("number of testing questions: " + processReader.testInstances.flatMap(_.questions).length)
        println("training instances: " + processReader.trainingInstances.length)
        println("testing instances: " + processReader.testInstances.length)
        println("non-true-false instances: " + allParagraphs.filterNotTrueFalse.flatMap(_.questions).length)
        println("non-true-false training instances: " + processReader.trainingInstances.filterNotTrueFalse.flatMap(_.questions).length)
        println("non-true-false testing instances: " + processReader.testInstances.filterNotTrueFalse.flatMap(_.questions).length)
        println("training instances about order: " + processReader.trainingInstances.filterTemporals.flatMap(_.questions).length)
        println("testing instances about order: " + processReader.testInstances.filterTemporals.flatMap(_.questions).length)
        println("training / filterNotTemporals.filterNotTrueFalse: " + processReader.trainingInstances.filterNotTemporals.filterNotTrueFalse.flatMap(_.questions).length)
        println("testing / filterNotTemporals.filterNotTrueFalse: " + processReader.testInstances.filterNotTemporals.filterNotTrueFalse.flatMap(_.questions).length)
      case 4 =>
        // evaluate processBank
        //        evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterTemporals, textILPSolver)
        //        println("filterTemporals: ")

        //      evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterCauseQuestions, textILPSolver)
        //      println("filterCauseQuestions: ")

        //      evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterCResultQuestions, textILPSolver)
        //      println("filterCResultQuestions: ")

        //     evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterWhatDoesItDo, textILPSolver)
        //     println("filterWhatDoesItDo: ")
        //
        evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals, textILPSolver)
        evaluateTextSolverOnProcessBank(processReader.testInstances.filterNotTrueFalse.filterNotTemporals, textILPSolver)

      // println("no-temporals/no true or false: ")
      //
      //      evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals.filterNotWhatDoesItDo.filterNotCResultQuestions, textILPSolver)
      //      println("no-temporals/no true or false/filterNotWhatDoesItDo.filterNotCResultQuestions")
      //      (0.12 to 0.5 by 0.02).foreach { weight =>
      //        val newParams = params.copy(minParagraphToQuestionAlignmentScore = weight)
      //        lazy val solver = new TextILPSolver(annotationUtils, verbose = false, newParams)
      //        evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals, solver)
      //        println("no-temporals/no true or false/weight: " + weight)
      //        println("-------------------")
      //      }
      // evaluateTextSolverOnProcessBank(processReader, slidingWindowSolver)
      case 5 =>
        import ResultJson._
        // write processBank on disk as json
        import java.io._
        val pw = new PrintWriter(new File("processBank-test3-filterNotTemporals.filterNotTrueFalse.json"))
        val json = Json.toJson(processReader.testInstances.filterNotTemporals.filterNotTrueFalse).toString
        pw.write(json)
        pw.close()

      case 6 =>
        evaluateTextSolverOnRegentsPerReasoningMethod(SolverUtils.publicTrain, textILPSolver)
        evaluateTextSolverOnRegentsPerReasoningMethod(SolverUtils.eigthGradeTrain, textILPSolver)
      case 7 =>
        // evaluate other solvers on regents
        evaluateTextSolverOnRegents(SolverUtils.regentsTrain, luceneSolver)
        evaluateTextSolverOnRegents(SolverUtils.publicTrain, luceneSolver)
        evaluateTextSolverOnRegents(SolverUtils.publicTest, luceneSolver)
        evaluateTextSolverOnRegents(SolverUtils.publicTrain, salienceSolver)
        evaluateTextSolverOnRegents(SolverUtils.publicTest, salienceSolver)
      case 55 =>
        // write the bioProcess questions on disk, as well as their predictions
        import java.io._
        val pw = new PrintWriter(new File("processBank-train.tsv"))
        val qAndpPairs = processReader.trainingInstances.flatMap { p => p.questions.map(q => (q, p)) }
        qAndpPairs.zipWithIndex.foreach {
          case ((q, p), idx) =>
            println("==================================================")
            println("Processed " + idx + " out of " + qAndpPairs.size)
            val candidates = q.answers.map(_.answerText)
            val correctIndex = q.correctIdxOpt.get
            //val (selected, explanation) = textILPSolver.solve(q.questionText, candidates, p.context)
            val correctLabel = q.answers(correctIndex).answerText
            //val score = SolverUtils.assignCredit(selected, correctIndex, candidates.length)
            //          pw.write(s"${q.questionText}\t${candidates.mkString("//")}\t${p.context}\t$correctIndex\t$score\n")
            pw.write(s"${q.questionText}\t${candidates.mkString("//")}\t${p.context}\t$correctIndex\n")
        }
        pw.close()
      case 73 =>
        val str1 = "This is a sentence. It is made of words"
        val str2 = "This sentence is similar. It has almost the same words"
        val metric = StringMetrics.cosineSimilarity()
        val metric2 = StringMetrics.blockDistance()
        val metric3 = StringMetrics.damerauLevenshtein()
        val metric4 = StringMetrics.dice()
        val metric5 = StringMetrics.euclideanDistance()
        val metric6 = StringMetrics.generalizedJaccard()
        val metric7 = StringMetrics.jaro()
        val metric8 = StringMetrics.jaroWinkler()
        val metric9 = StringMetrics.needlemanWunch()
        println("result: " + metric.compare(str1, str2))
        println("result: " + metric2.compare(str1, str2))
        println("result: " + metric3.compare(str1, str2))
        println("result: " + metric4.compare(str1, str2))
        println("result: " + metric5.compare(str1, str2))
        println("result: " + metric6.compare(str1, str2))
        println("result: " + metric7.compare(str1, str2))
        println("result: " + metric8.compare(str1, str2))
        println("result: " + metric9.compare(str1, str2))
      case 80 =>
        // print the questions on which Lucene is correct but TextILP is wrong
        SolverUtils.regentsTrain.map {
          case (question, options, correct) =>
            val knowledgeSnippet = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, 8).mkString(" ")
            val (selected, results) = if (knowledgeSnippet.trim.nonEmpty) {
              textILPSolver.solve(question, options, knowledgeSnippet)
            } else {
              println("Error: knowledge not found .  .  .")
              // choose all of them
              options.indices -> EntityRelationResult()
            }
            val textIlpScore = SolverUtils.assignCredit(selected, correct.head - 'A', options.length)
            val (luceneSelected, luceneResults) = luceneSolver.solve(question, options, knowledgeSnippet)
            val luceneScore = SolverUtils.assignCredit(luceneSelected, correct.head - 'A', options.length)
            (textIlpScore, luceneScore, question)
        }.foreach {
          case (textIlpScore, luceneScore, question) =>
            if (textIlpScore < luceneScore) {
              println(question)
              println("lucene: " + luceneScore + " / " + textIlpScore)
              println("-------")
            }
        }

      case 93 =>
        // read the json predictions of the BiDaF and evaluate
        processAnswers("output/processBank-train5-output.json", processReader.trainingInstances.filterNotTemporals.filterNotTrueFalse)
        processAnswers("output/processBank-test5-output.json", processReader.testInstances.filterNotTemporals.filterNotTrueFalse)

        def processAnswers(ansFile: String, questionSet: List[Paragraph]) = {
          println("ansFile: " + ansFile)
          val lines = Source.fromFile(ansFile).getLines.toList.mkString
          val json = Json.parse(lines)
          val answerMap = json.as[JsObject].fields.map { case (a, b) => a.toString -> b.toString }.toMap
          val scores: Seq[Double] = questionSet.zipWithIndex.flatMap {
            case (p, pIdx) =>
              p.questions.zipWithIndex.map {
                case (q, qIdx) =>
                  val id: String = s"$pIdx-$qIdx"
                  println(" === id: " + id)
                  val ans: String = answerMap.getOrElse(id, {
                    println(" =====> The id did not found!!!!")
                    "random"
                  })
                  val selectedIdx = q.answers.zipWithIndex.map {
                    case (a, idx) =>
                      idx -> TextILPSolver.offlineAligner.scoreCellCell(a.answerText, ans)
                  }.maxBy(_._2)._1
                  SolverUtils.assignCredit(Seq(selectedIdx), q.correctIdxOpt.get, 2)
              }
          }
          println("average score: " + scores.sum / scores.length + "  - size: " + scores.length)
        }
      case 95 =>
        // read Vivek's data
        val predictions = Source.fromFile(new File(Constants.vivekPredictonsFile)).getLines().toList.drop(1).map { line =>
          val split = line.split("\t")
          val pid = split(1)
          val question = split(2)
          val resultStr = split(4)
          val result = if (resultStr.contains("Incorrect")) 0.0 else 1.0
          (pid, result, question)
        }
        println(Constants.vivekTestParagraphs.slice(0, 50))
        println(predictions.slice(0, 50))
        val (test, train) = predictions.partition(x => Constants.vivekTestParagraphs.contains(x._1))
        /*        val testResult = test.unzip3._2.sum / test.unzip3._2.length
        val trainResult = train.unzip3._2.sum / train.unzip3._2.length
        println("test result: " + testResult + "  testResult.length: " + test.size)
        println("train result: " + trainResult + "  trainResult.length: " + train.size)
        val testNonCauseNonTemporal = test.filter(q => !q._3.isCauseQuestion && !q._3.isTemporal)
        val trainNonCauseNonTemporal = train.filter(q => !q._3.isCauseQuestion && !q._3
          .isTemporal)
        println("test result: " + testNonCauseNonTemporal.unzip3._2.sum / testNonCauseNonTemporal.unzip3._2.length +
          "  testResult.length: " + testNonCauseNonTemporal.size)
        println("train result: " + trainNonCauseNonTemporal.unzip3._2.sum / trainNonCauseNonTemporal.unzip3._2.length +
          "  trainResult.length: " + trainNonCauseNonTemporal.size)*/

        //        println("----------")
        //        println("train: \n" + train.mkString("\n"))
        //        println("----------")
        //        println("test: \n" + test.mkString("\n"))

        // get missing documents
        val all = (1 to 210).map(i => "p" + i)
        val vivekDocs = predictions.map(_._1).distinct
        println("missing docs: " + (all diff vivekDocs))

      case 102 =>
        val question = "Which force causes a marble to sink to the bottom of a glass of water?"
        val options = Seq("gravity", "friction", "magnetism", "electricity")
        val knowledgeLength = 8
        val rawSentences = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, knowledgeLength).mkString(". ")
        println(annotationUtils.dropRedundantSentences(rawSentences))

      case 111 =>
        def writeFeaturesOnDisk(dataset: Seq[(String, Seq[String], String)]): Unit = {
          val f = s"output/featureVectors-${dataset.length}.txt"
          IOUtils.rm(f)
          val max = dataset.length
          dataset.zipWithIndex.foreach {
            case ((question, options, correct), idx) =>
              println(s"Processing $idx out of $max")
              val knowledgeSnippet = {
                val rawSentences = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, 8).mkString(". ")
                annotationUtils.dropRedundantSentences(rawSentences)
              }
              println("knowledge: " + knowledgeSnippet)
              if (knowledgeSnippet.trim != "") {
                println("solving it . . . ")
                val out = textILPSolver.extractFeatureVectorForQuestionWithCorrectLabel(question, options, knowledgeSnippet, correct.head - 'A').map(_.mkString(", ")).mkString("\n")
                val resultFile = new FileWriter(new File(f), true)
                resultFile.write(out + "\n")
                resultFile.close()
              }
          }
        }

        def writeProcessBankData(list: List[Paragraph]) = {
          val qAndpPairs = list.flatMap { p => p.questions.map(q => (q, p)) }
          val max = qAndpPairs.length
          val resultFile = new PrintWriter(new File(s"output/featureVectors-${max}.txt"))
          qAndpPairs.zipWithIndex.foreach {
            case ((q, p), idx) =>
              println(s"Processing $idx out of $max")
              val knowledgeSnippet = p.context
              val question = q.questionText
              val options = q.answers.map(_.answerText)

              if (knowledgeSnippet.trim != "") {
                println("solving it . . . ")
                val out = textILPSolver.extractFeatureVectorForQuestionWithCorrectLabel(question, options, knowledgeSnippet, q.correctIdxOpt.get).map(_.mkString(", ")).mkString("\n")
                resultFile.write(out + "\n")
              }
          }
          resultFile.close()
        }

        def writeFeatureHeaderFile(): Unit = {
          val types = Seq(SimpleMatching, WhatDoesItDoRule, CauseRule, SRLV1Rule, VerbSRLandPrepSRL, SRLV1ILP, VerbSRLandCoref, SRLV2Rule, SRLV3Rule, VerbSRLandCommaSRL)
          val srlViewsAll = Seq(ViewNames.SRL_VERB, TextILPSolver.curatorSRLViewName, TextILPSolver.pathLSTMViewName)
          val bw = new BufferedWriter(new FileWriter("output/headerFile.arff"))
          bw.write("@RELATION Components\n")
          types.foreach { t =>
            val srlViewTypes = if (t == CauseRule || t == WhatDoesItDoRule || t == SimpleMatching) Seq(TextILPSolver.pathLSTMViewName) else srlViewsAll
            srlViewTypes.foreach { srlVu =>
              bw.write(s"@ATTRIBUTE $t-${srlVu}_BINARY NUMERIC\n")
              bw.write(s"@ATTRIBUTE $t-${srlVu}_OBJ NUMERIC\n")
              bw.write(s"@ATTRIBUTE $t-${srlVu}_BIN_VAR_COUNT NUMERIC\n")
              bw.write(s"@ATTRIBUTE $t-${srlVu}_CONS_COUNT NUMERIC\n")
              bw.write(s"@ATTRIBUTE $t-${srlVu}_OBJ_OVR_VAR_COUNT NUMERIC\n")
              bw.write(s"@ATTRIBUTE $t-${srlVu}_OBJ_OVR_CONS_COUNT NUMERIC\n")
            }
          }
          bw.write("@ATTRIBUTE output {1.0, 0.0}\n")
          bw.write("@DATA\n")
          bw.close()
        }

        writeFeatureHeaderFile()

      //        writeProcessBankData(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals)
      //        writeFeaturesOnDisk(SolverUtils.regentsTrain)
      //        writeFeaturesOnDisk(SolverUtils.eigthGradeTrain)
      //        writeFeaturesOnDisk(SolverUtils.publicTrain)
      case 112 =>
        def cacheOnDiskAi2(dataset: Seq[(String, Seq[String], String)]): Unit = {
          val max = dataset.length
          dataset.zipWithIndex.foreach {
            case ((question, options, correct), idx) =>
              println(s"Processing $idx out of $max")
              val knowledgeSnippet = {
                val rawSentences = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, 8).mkString(". ")
                annotationUtils.dropRedundantSentences(rawSentences)
              }

              println("knowledge: " + knowledgeSnippet)
              annotationUtils.annotateWithEverything(question, withFillInBlank = true)
              if (knowledgeSnippet.trim != "") {
                annotationUtils.annotateWithEverything(knowledgeSnippet, withFillInBlank = false)
              }
          }
        }

        def cacheOnDisk(list: List[Paragraph]) = {
          val qAndpPairs = list.flatMap { p => p.questions.map(q => (q, p)) }
          val max = qAndpPairs.length
          qAndpPairs.zipWithIndex.foreach {
            case ((q, p), idx) =>
              println(s"Processing $idx out of $max")
              val knowledgeSnippet = p.context
              val question = q.questionText
              val options = q.answers.map(_.answerText)
              annotationUtils.annotateWithEverything(q.questionText, withFillInBlank = true)
              if (knowledgeSnippet.trim != "") {
                annotationUtils.annotateWithEverything(p.context, withFillInBlank = false)
              }
          }
        }

        //        cacheOnDiskAi2(SolverUtils.eigthGradeTrainPublic)
        //          println("---> eigthGradeTrainPublic \n --------------")
        cacheOnDiskAi2(SolverUtils.eigthGradeTestPublic)
        println("---> eigthGradeTestPublic \n --------------")

      //        cacheOnDisk(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals)
      //        cacheOnDiskAi2(SolverUtils.regentsTrain)
      //        cacheOnDiskAi2(SolverUtils.eigthGradeTrain)
      //        cacheOnDiskAi2(SolverUtils.publicTrain)

      //        cacheOnDisk(processReader.testInstances.filterNotTrueFalse.filterNotTemporals)
      //        cacheOnDiskAi2(SolverUtils.regentsTest)
      //        cacheOnDiskAi2(SolverUtils.eigthGradeTest)
      //        cacheOnDiskAi2(SolverUtils.publicTest)
      case 114 =>
        // train a Weka classifier
        val loader = new ArffLoader()
        loader.setFile(new File("output/featuresCombined.arff"))
        val train = loader.getDataSet
        //println("structure.numAttributes(): " + train.numAttributes())
        train.setClassIndex(train.numAttributes() - 1)

        val lg = new Logistic
        lg.buildClassifier(train)
        println(lg.coefficients())
        println(lg.getTechnicalInformation)
        println(lg.getOptions)

        val eval = new Evaluation(train)
        eval.evaluateModel(lg, train)
        println(eval.toMatrixString("=== Confusion matrix for fold " + 0 + "/" + 10 + " ===\n"))
        println("eval.fMeasure(0): " + eval.fMeasure(0))
        println("eval.fMeasure(1): " + eval.fMeasure(1))

        val oos = new ObjectOutputStream(new FileOutputStream("output/logistic.model"))
        oos.writeObject(lg)
        oos.flush()
        oos.close()

      case 115 =>
        def evaluateAi2(dataset: Seq[(String, Seq[String], String)]): Unit = {
          val max = dataset.length
          val scores = dataset.zipWithIndex.map {
            case ((question, options, correct), idx) =>
              println(s"Processing $idx out of $max")
              val knowledgeSnippet = {
                val rawSentences = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, 8).mkString(". ")
                annotationUtils.dropRedundantSentences(rawSentences)
              }
              println("knowledge: " + knowledgeSnippet)
              val correctIndex = correct.head - 'A'
              val selected = if (knowledgeSnippet.trim != "") {
                println("solving it . . . ")
                try {
                  val selected = Seq(textILPSolver.predictWithWekaClassifier(question, options, knowledgeSnippet))
                  val f = new FileWriter(s"output/predictionPerQuestion-${max}.txt", true)
                  f.write(question + "\t" + selected.head + "\t" + correctIndex + "\n")
                  f.close()
                  //                  val features = textILPSolver.extractFeatureVectorForQuestionWithCorrectLabel(question, options, knowledgeSnippet, correct.head - 'A').map(_.mkString(", ")).mkString("\n")
                  //                  val f2 = new FileWriter(s"output/features-${max}.txt", true)
                  //                  f2.write(features + "\n")
                  //                  f2.close()
                  selected
                } catch {
                  case e: Exception =>

                    e.printStackTrace()
                    Seq.empty
                }
              } else {
                Seq.empty
              }
              SolverUtils.assignCredit(selected, correctIndex, options.length)
          }
          println(" --> Average score: " + {
            scores.sum / scores.size
          })
        }

        def evluateProcessbankData(list: List[Paragraph]) = {
          val qAndpPairs = list.flatMap { p => p.questions.map(q => (q, p)) }
          val max = qAndpPairs.length
          val scores = qAndpPairs.zipWithIndex.map {
            case ((q, p), idx) =>
              println(s"Processing $idx out of $max")
              val knowledgeSnippet = p.context
              val question = q.questionText
              val options = q.answers.map(_.answerText)
              val selected = if (knowledgeSnippet.trim != "") {
                println("solving it . . . ")
                val selected = Seq(textILPSolver.predictWithWekaClassifier(question, options, knowledgeSnippet))
                val f = new FileWriter(s"output/predictionPerQuestion-ProcessBank-${max}.txt", true)
                f.write(question + "\t" + selected + "\t" + q.correctIdxOpt.get + "\n")
                f.close()
                selected
              } else {
                Seq.empty
              }
              SolverUtils.assignCredit(selected, q.correctIdxOpt.get, options.length)
          }
          println(" --> Average score: " + {
            scores.sum / scores.size
          })
        }

        //        evaluateAi2(SolverUtils.eigthGradeTrainPublic)
        //        println("---> eigthGradeTrainPublic \n --------------")
        //        evaluateAi2(SolverUtils.eigthGradeTestPublic.take(40))
        //        println("---> eigthGradeTestPublic \n --------------")

        //        evaluateAi2(SolverUtils.regentsPerturbed)
        //        println("---> regentsPerturbed \n --------------")

        //        evaluateAi2(SolverUtils.regentsTrain)
        //        println("regents train \n --------------")
        //        evaluateAi2(SolverUtils.regentsTest.slice(0, 20))
        //        println("--> regentsTest \n --------------")
        //        evaluateAi2(SolverUtils.eigthGradeTrain)
        //        println("--> eigthGradeTrain \n --------------")
        //        evaluateAi2(SolverUtils.eigthGradeTest)
        //        println("---> eigthGradeTest \n --------------")
        //        evluateProcessbankData(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals)
        //        println("---> processReader train \n --------------")
        evluateProcessbankData(processReader.testInstances.filterNotTrueFalse.filterNotTemporals.take(25))
        println("---> processReader test \n --------------")
        //        evaluateAi2(SolverUtils.publicTrain)
        //        println("---> publicTrain \n --------------")
        evaluateAi2(SolverUtils.publicTest.take(40))
        println("---> publicTest \n --------------")

      case 116 =>
        import SquadJsonPattern._
        // save ai2 datasets using SquadJsonPattern
        // note that the above import from SquadJsonPattern is key
        val paragraphs: Seq[Paragraph] = SolverUtils.convertAi2DatasetIntoStandardFormat(SolverUtils.publicTrain, annotationUtils)
        println("size of the dataset: " + paragraphs.length)
        val json = Json.toJson(paragraphs).toString
        val pw = new PrintWriter(new File("output/public-train6.json"))
        pw.write(json)
        pw.close()

      case 117 =>
        // read the json predictions of the BiDaF and evaluate
        processAnswers("output/regents-train1-output.json", SolverUtils.convertAi2DatasetIntoStandardFormat(SolverUtils.regentsTrain, annotationUtils))
        processAnswers("output/regents-test4-output.json", SolverUtils.convertAi2DatasetIntoStandardFormat(SolverUtils.regentsTest, annotationUtils))
        processAnswers("output/regents-8th-train1-output.json", SolverUtils.convertAi2DatasetIntoStandardFormat(SolverUtils.eigthGradeTrain, annotationUtils))
        processAnswers("output/regents-8th-test1-output.json", SolverUtils.convertAi2DatasetIntoStandardFormat(SolverUtils.eigthGradeTest, annotationUtils))
        processAnswers("output/public-train6-output.json", SolverUtils.convertAi2DatasetIntoStandardFormat(SolverUtils.publicTrain, annotationUtils))
        processAnswers("output/public-test2-output.json", SolverUtils.convertAi2DatasetIntoStandardFormat(SolverUtils.publicTest, annotationUtils))

        def processAnswers(ansFile: String, questionSet: Seq[Paragraph]) = {
          println("ansFile: " + ansFile)
          val lines = Source.fromFile(ansFile).getLines.toList.mkString
          val json = Json.parse(lines)
          val answerMap = json.as[JsObject].fields.map { case (a, b) => a.toString -> b.toString }.toMap
          println("answerMap: " + answerMap.keySet.toSeq.sorted)
          val scores: Seq[Double] = questionSet.zipWithIndex.flatMap {
            case (p, pIdx) =>
              p.questions.zipWithIndex.map {
                case (q, qIdx) =>
                  val id: String = s"$pIdx-$qIdx"
                  println(" === id: " + id)
                  val ans: String = answerMap.getOrElse(id, {
                    println(" =====> The id did not found!!!!")
                    "random"
                  })
                  val selectedIdx = q.answers.zipWithIndex.map {
                    case (a, idx) =>
                      idx -> TextILPSolver.offlineAligner.scoreCellCell(a.answerText, ans)
                  }.maxBy(_._2)._1
                  SolverUtils.assignCredit(Seq(selectedIdx), q.correctIdxOpt.get, q.answers.length)
              }
          }
          println(s"File: ${ansFile}  / average score: " + scores.sum / scores.length + "  - size: " + scores.length)
        }

      case 119 =>
      // finding overlap with lucene predictions

    }
  }
}