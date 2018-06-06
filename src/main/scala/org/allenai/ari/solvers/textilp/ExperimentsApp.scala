package org.allenai.ari.solvers.textilp

import java.io._

import org.allenai.ari.solvers.bioProccess.ProcessBankReader
import org.allenai.ari.solvers.bioProccess.ProcessBankReader._
import org.allenai.ari.solvers.textilp.solvers.TextILPSolver
import org.allenai.ari.solvers.textilp.utils._
import org.rogach.scallop._

import scala.io.Source

object ExperimentsApp {
  lazy val annotationUtils = new AnnotationUtils()
  lazy val textILPSolver = new TextILPSolver(annotationUtils, verbose = false, SolverUtils.params)

  class ArgumentParser(args: Array[String]) extends ScallopConf(args) {
    val experimentType: ScallopOption[Int] = opt[Int]("type", descr = "Experiment type", required = true)
    verify()
  }

  def evaluateTextSolverOnRegents(dataset: Seq[(String, Seq[String], String)], textSolver: TextILPSolver, knowledgeLength: Int = 8) = {
    val resultFile = new PrintWriter(new File(s"output/results-$textSolver-2length${dataset.length}-knowledgeLength${knowledgeLength}.txt"))
    import java.io._
    // use false if you don't it to write things on disk
    val predictionsFileOpt = if (false) {
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
          textSolver.solve(question, options, knowledgeSnippet)
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
        if (false && score < 1.0) {
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

  def evaluateTextSolverOnProcessBank(list: List[Paragraph], textSolver: TextILPSolver) = {
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
        val (selected, explanation) = try {
          textSolver.solve(q.questionText, candidates, p.context)
        } catch {
          case e: Exception =>
            e.printStackTrace
            Seq.empty -> EntityRelationResult()
        }
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
        val question = "Which force causes a marble to sink to the bottom of a glass of water?"
        val options = Seq("gravity", "friction", "magnetism", "electricity")
        val knowledgeLength = 8
        val rawSentences = SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, knowledgeLength).mkString(". ")
        println(annotationUtils.dropRedundantSentences(rawSentences))

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
        // evaluating science exams

        // evaluateTextSolverOnRegents(SolverUtils.eighthGradeTrain, textILPSolver)
        // println("==== eighthGradeTrain  ")
        //evaluateTextSolverOnRegents(SolverUtils.eighthGradeTest, textILPSolver)
        //println("==== eighthGradeTest ")

        // evaluateTextSolverOnRegents(SolverUtils.eighthGradeTrainPublic, textILPSolver)
        // println("==== eighthGradeTrainPublic  ")
        //evaluateTextSolverOnRegents(SolverUtils.eighthGradeTestPublic, textILPSolver)
        //println("==== eighthGradeTestPublic ")

        // evaluateTextSolverOnRegents(SolverUtils.regentsTrain, textILPSolver)
        // println("==== regents train  ")
        evaluateTextSolverOnRegents(SolverUtils.regentsTest, textILPSolver)
        println("==== regents test  ")

        // evaluateTextSolverOnRegents(SolverUtils.regentsPerturbed, textILPSolver)
        // println("==== regents perturbed  ")
        // evaluateTextSolverOnRegents(SolverUtils.publicTrain, textILPSolver)
        // println("==== public train ")
        // evaluateTextSolverOnRegents(SolverUtils.publicDev, textILPSolver)
        // println("==== public dev ")
        // evaluateTextSolverOnRegents(SolverUtils.publicTest.zipWithIndex.collect { case (a, i) if i != 130 => a }, textILPSolver)
        // println("==== public test ")
        // evaluateTextSolverOnRegents(SolverUtils.omnibusTrain, textILPSolver)
        // println("==== omnibus train ")
        // evaluateTextSolverOnRegents(SolverUtils.omnibusTest, textILPSolver)
        // println("==== omnibus test ")

        // evaluateTextSolverOnRegents(SolverUtils.squid04test, textILPSolver)
        // println("==== squid04test ")

        // evaluateTextSolverOnRegents(SolverUtils.squidAdditionalTest, textILPSolver)
        // println("==== squidAdditionalTest ")

        // evaluateTextSolverOnRegents(SolverUtils.squidChallengeTest, textILPSolver)
        // println("==== squidChallengeTest ")

      case 5 =>
        // evaluate processBank
        // evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterTemporals, textILPSolver)
        // println("filterTemporals: ")

        // evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterCauseQuestions, textILPSolver)
        // println("filterCauseQuestions: ")

        // evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterCResultQuestions, textILPSolver)
        // println("filterCResultQuestions: ")

        // evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterWhatDoesItDo, textILPSolver)
        // println("filterWhatDoesItDo: ")

        // evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals, textILPSolver)
        evaluateTextSolverOnProcessBank(processReader.testInstances.filterNotTrueFalse.filterNotTemporals, textILPSolver)

         case 6 =>
        // process multirc questions
        val ps = Paragraph2.readJson5("/home/danielk/splitv2/dev_83-fixedIds.json") ++
          Paragraph2.readJson5("/home/danielk/splitv2/test_1_83-fixedIds.json") ++
          Paragraph2.readJson5("/home/danielk/splitv2/test_2_83-fixedIds.json") ++
          Paragraph2.readJson5("/home/danielk/splitv2/test_3_83-fixedIds.json") ++
          Paragraph2.readJson5("/home/danielk/splitv2/test_4_83-fixedIds.json") ++
          Paragraph2.readJson5("/home/danielk/splitv2/train_456-fixedIds.json")

        val reg = "<b>Sent \\d{1,2}: </b>".r

        def cacheOnDiskAi2(list: Seq[Paragraph2]): Unit = {
          val max = list.length
          list.zipWithIndex.foreach {
            case (p, idx) =>
              println(s"Processing $idx out of $max")
              if (idx > 8) {
                try {
                  val sentences = reg.split(p.text).drop(1)
                  val knowledgeSnippet = sentences.map(_.trim().replace("<br>", " ")).mkString
                  println("knowledge: " + knowledgeSnippet)
                  p.questions.zipWithIndex.foreach {
                    case (q, qIdx) =>
                      q.answers.zipWithIndex.foreach {
                        case (a, aIdx) =>
                          try {
//                             textILPSolver.preprocessQuestionData(q.text, Seq(a.text), knowledgeSnippet)
                             val f = new FileWriter(s"output/textilp-multirc-june6.txt", true)
                             val out = textILPSolver.predictAllCandidatesWithWekaClassifier(q.text, Seq(a.text), knowledgeSnippet)
                             val score = out._1
                             f.write(p.id + "\t" + qIdx + "\t" + aIdx + "\t" + score + "\n")
                             f.close()
                          } catch {
                            case e: Exception =>
                              println(s"solving question with id ${p.id} - ${qIdx} failed . . . ")
                              e.printStackTrace()
                          }
                      }
                  }
                } catch {
                  case e: Exception =>
                    println("cache failed . . . ")
                    e.printStackTrace()
                }
              }
          }
        }

        cacheOnDiskAi2(ps)


      case 7 =>
        val ps = Paragraph2.readJson5("/Users/daniel/ideaProjects/hard-qa/splitv2/dev_83-fixedIds.json")

        val reg = "<b>Sent \\d{1,2}: </b>".r

        def cacheOnDiskAi2(list: Seq[Paragraph2]): Unit = {
          val max = list.length
          list.zipWithIndex.foreach {
            case (p, idx) =>
              //if(idx > 10) {
              println(s"Processing $idx out of $max")
              p.questions.zipWithIndex.foreach {
                case (q, qIdx) =>
                  println("----> q")
                  q.answers.zipWithIndex.foreach {
                    case (a, aIdx) =>
                      //Thread.sleep(500)
                      try {
                        val results = SolverUtils.staticCacheLucene(q.text, a.text, 100)
                        val maxScore = if (results.nonEmpty) results.maxBy(_._2)._2 else 0.0
                        a.scores += ("lucene-world" -> maxScore)
                      } catch {
                        case e: Exception =>
                          e.printStackTrace()
                          Thread.sleep(2000)
                      }
                  }
              }
            //}
          }
        }

        cacheOnDiskAi2(ps)
        Paragraph2.writeJson4("/Users/daniel/ideaProjects/hard-qa/split/dev_83-with-lucene-local.json", ps)

      
      
    }
  }
}
