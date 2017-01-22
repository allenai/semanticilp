package org.allenai.ari.solvers.textilp

import java.io.File

import edu.illinois.cs.cogcomp.McTest.MCTestBaseline
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.utilities.DummyTextAnnotationGenerator
import edu.illinois.cs.cogcomp.core.utilities.configuration.ResourceManager
import org.allenai.ari.solvers.bioProccess.ProcessBankReader
import org.allenai.ari.solvers.squad.SQuADReader
import org.allenai.ari.solvers.textilp.solvers.SlidingWindowSolver
import play.api.libs.json.Json

import org.allenai.ari.solvers.squad.SquadClassifierUtils._
import org.allenai.ari.solvers.squad.{CandidateGeneration, SquadClassifier, SquadClassifierUtils, TextAnnotationPatternExtractor}
import org.allenai.ari.solvers.textilp.alignment.AlignmentFunction
import org.allenai.ari.solvers.textilp.solvers.{LuceneSolver, SalienceSolver, TextILPSolver, TextSolver}
import org.allenai.ari.solvers.textilp.utils.WikiUtils.WikiDataProperties
import org.allenai.ari.solvers.textilp.utils._
import org.rogach.scallop._

import scala.collection.JavaConverters._
import ProcessBankReader._
import org.allenai.ari.solvers.textilp.ResultJson._

object ExperimentsApp {
  lazy val annotationUtils = new AnnotationUtils()
  lazy val textILPSolver = new TextILPSolver(annotationUtils)
  lazy val salienceSolver = new SalienceSolver()
  lazy val luceneSolver = new LuceneSolver()
  lazy val slidingWindowSolver = new SlidingWindowSolver()

  class ArgumentParser(args: Array[String]) extends ScallopConf(args) {
    val experimentType: ScallopOption[Int] = opt[Int]("type", descr = "Experiment type", required = true)
    verify()
  }

  def testQuantifier(): Unit = {
    val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "",
      "Helicopters will patrol the temporary no-fly zone around New Jersey's MetLife Stadium Sunday, with F-16s based in Atlantic City ready to be scrambled if an unauthorized aircraft does enter the restricted airspace.\n\nDown below, bomb-sniffing dogs will patrol the trains and buses that are expected to take approximately 30,000 of the 80,000-plus spectators to Sunday's Super Bowl between the Denver Broncos and Seattle Seahawks.\n\nThe Transportation Security Administration said it has added about two dozen dogs to monitor passengers coming in and out of the airport around the Super Bowl.\n\nOn Saturday, TSA agents demonstrated how the dogs can sniff out many different types of explosives. Once they do, they're trained to sit rather than attack, so as not to raise suspicion or create a panic.\n\nTSA spokeswoman Lisa Farbstein said the dogs undergo 12 weeks of training, which costs about $200,000, factoring in food, vehicles and salaries for trainers.\n\nDogs have been used in cargo areas for some time, but have just been introduced recently in passenger areas at Newark and JFK airports. JFK has one dog and Newark has a handful, Farbstein said.")
    annotationUtils.pipelineService.addView(ta, ViewNames.QUANTITIES)
    println(ta)
    println(ta.getAvailableViews)
    println(ta.getView(ViewNames.QUANTITIES))
    println(ta.getView(ViewNames.QUANTITIES).getConstituents.asScala.filter(_.getLabel.contains("Date")))
    println(ta.getView(ViewNames.QUANTITIES).getConstituents.asScala.map(c => c.getSurfaceForm -> c.getLabel))
  }

  def testPipelineAnnotation(): Unit = {
    val ta = annotationUtils.pipelineService.createAnnotatedTextAnnotation("", "",
      "this is a sample senrence that needs to be update with 20 pipelines in Illinois. ")
    println(ta)
    println(ta.getAvailableViews)
  }

  def dumpSQuADQuestionsOnDisk(reader: SQuADReader) = {
    import java.io._
    val pw = new PrintWriter(new File("squadQuestions.txt" ))
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        ins.paragraphs.foreach { p =>
          pw.write("P: " + p.context + "\n")
          p.questions.foreach { q =>
            pw.write("Q: " + q.questionText + "\n")
          }
        }
    }
    pw.close()
  }

  def testCuratorAnnotation() = {
    val ta = annotationUtils.curatorService.createBasicTextAnnotation("", "",
      "This is a sample sentence. Barak Obama is the president of US. Lake Tahoe is a nice place to visit. I like the blue skye. ")
    annotationUtils.curatorService.addView(ta, ViewNames.WIKIFIER)
    println(ta.getAvailableViews)
  }

  //TODO if "the" is among the candidate answrs, drop it and make it another candidate
  //TODO capture alphabetical numbers too, like "six"
  def generateCandiateAnswers(reader: SQuADReader): Unit = {
    var found: Int = 0
    var notFound: Int = 0
    reader.instances.zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.foreach { p =>
          p.contextTAOpt match {
            case None => throw new Exception("The instance does not contain annotation . . . ")
            case Some(annotation) =>
              val candidateAnswers = CandidateGeneration.getCandidateAnswer(annotation)
              p.questions.foreach { q =>
                val goldAnswers = q.answers.map(_.answerText)
                if (goldAnswers.exists(candidateAnswers.contains)) {
                  println(" --> found ")
                  found = found + 1
                } else {
                  notFound = notFound + 1
                  println(" --> not found ")
                  println("Question: " + q)
                  println("CandidateAnswers: " + candidateAnswers)
                  println("context = " + p.context)
                }
              }
          }
        }
    }
    println("found: " + found + "\nnot-found: " + notFound)
  }

  def evaluateDataSetWithRemoteSolver(reader: SQuADReader, solver: String): Unit = {
    reader.instances.slice(0, 3).zipWithIndex.foreach {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.slice(0, 3).foreach { p =>
          p.contextTAOpt match {
            case None => throw new Exception("The instance does not contain annotation . . . ")
            case Some(annotation) =>
              val candidateAnswers = CandidateGeneration.getCandidateAnswer(annotation).toSeq
              p.questions.foreach { q =>
                val goldAnswers = q.answers.map(_.answerText)
                val perOptionScores = SolverUtils.handleQuestionWithManyCandidates(q.questionText, candidateAnswers, solver)
                println("q.questionText = " + q.questionText)
                println("gold = " + goldAnswers)
                //println("predicted = " + perOptionScores.sortBy(-_._2))
                println("---------")
              }
          }
        }
    }
  }

  def testAlignmentScore() = {
    println("Testing the alignment . . . ")
    println("ent(moon, moon) = " + AlignmentFunction.entailment.entail(Seq("moon"), Seq("moon")))
    println("ent(moon, sun) = " + AlignmentFunction.entailment.entail(Seq("moon"), Seq("sun")))
    println("ent(sun, moon) = " + AlignmentFunction.entailment.entail(Seq("sun"), Seq("moon")))
  }

  def solveSampleQuestionWithTextILP() = {
    textILPSolver.solve(
      "A decomposer is an organism that",
      Seq("hunts and eats animals", "migrates for the winter",
        "breaks down dead plants and animals", "uses water and sunlight to make food"),
      "explanation:Decomposers: organisms that obtain energy by eating dead plant or animal matter. " +
        "Windy, cloudy, rainy, and cold are words that help describe\tfocus: deposition. " +
        "explanation:DECOMPOSER An organism that breaks down cells of dead plants and animals into simpler substances." +
        "explanation:The plants use sunlight, carbon dioxide, water, and minerals to make food that sustains themselves and other organisms in the forest."
    )
//    textILPSolver.solve(
//      "A decomposer",
//      Set("hunts ", "migrates for the winter",
//        "breaks down dead plants and animals", "uses water and sunlight to make food"),
//      "Decomposers"
//    )
  }

  def testElasticSearchSnippetExtraction() = {
    println(SolverUtils.extractParagraphGivenQuestionAndFocusWord("when is the best time of the year in New York city, especially when it snows or rains?", "Christmas", 3).mkString("\n"))
  }

  def extractKnowledgeSnippet() = {
    val question = "In New York State the longest period of daylight occurs during which month"
    val candidates = Seq("June", "March", "December", "September")
    candidates.foreach { focus =>
      println("Query: " + question + "  " + focus + " --> Result: " + SolverUtils.extractParagraphGivenQuestionAndFocusWord2(question, focus, 3).mkString(" "))
      //println("Q: In New York State --> " + SolverUtils.extractParagraphGivenQuestionAndFocusWord2("In New York State ", focus, 3))
      //println("Q: the longest period of daylight --> " +SolverUtils.extractParagraphGivenQuestionAndFocusWord2("the longest period of daylight", focus, 3))
      //println("----------")
    }
    println("Query: " + question + "  " + candidates + " --> Result: " + SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, candidates, 8).mkString(" "))
  }

  def testRemoteSolverWithSampleQuestion() = {
    SolverUtils.evaluateASingleQuestion("Which two observations are both used to describe weather? (A) like (B) the difference (C) events (D) temperature and sky condition", "tableilp")
  }

  def evaluateTextSolverOnRegents(dataset: Seq[(String, Seq[String], String)], textSolver: TextSolver) = {
    SolverUtils.printMemoryDetails()
    println("Starting the evaluation . . . ")
    val perQuestionScore = dataset.map{ case (question, options, correct) =>
      //println("collecting knolwdge . . . ")
//      val knowledgeSnippet = options.flatMap(focus => SolverUtils.extractParagraphGivenQuestionAndFocusWord(question, focus, 3)).mkString(" ")
//      val knowledgeSnippet = options.flatMap(focus => SolverUtils.extractParagraphGivenQuestionAndFocusWord2(question, focus, 3)).mkString(" ")
      val knowledgeSnippet = if(textSolver.isInstanceOf[TextILPSolver]) {
        SolverUtils.extractPatagraphGivenQuestionAndFocusSet3(question, options, 8).mkString(" ")
      }
      else {
        ""
      }
      //println("solving it . . . ")
      val (selected, _) = textSolver.solve(question, options, knowledgeSnippet)
      val score = SolverUtils.assignCredit(selected, correct.head - 'A', options.length)
      //println("Question: " + question + " / options: " + options  +  "   / selected: " + selected  + " / score: " + score)
      score
    }
    println("Average score: " + perQuestionScore.sum / perQuestionScore.size)
  }

  def evaluateTextSolverOnSquad(reader: SQuADReader, textSolver: TextSolver) = {
    val qAndpPairs = reader.instances.slice(0, 20).flatMap { i => i.paragraphs.slice(0,20).flatMap{p => p.questions.slice(0, 2).map(q => (q, p))}}.take(1000)
    val pairsGrouped = qAndpPairs.groupBy(_._2)
    val resultLists = pairsGrouped.zipWithIndex.flatMap{ case ((p, pairs), idx) =>
      val allCandidates = CandidateGeneration.getCandidateAnswer(p.contextTAOpt.get).filter(_.split(" ").length < 7)
      println("==================================================")
      println("Processed " + idx + " out of " + pairsGrouped.size)
      println("Paragraph: " + p.context)
      println("all candidates = " + allCandidates)
      pairs.map{ case (q, _) =>
        val goldCandidates = q.answers.map(_.answerText)
        val allCandidatesMinusCorrectOnes = scala.util.Random.shuffle(allCandidates).diff(goldCandidates.toSet)
        if(allCandidatesMinusCorrectOnes.size > 1) {
          val candidates = allCandidatesMinusCorrectOnes.slice(0, 5).toSeq :+ goldCandidates.head
          val correctIndex = math.min(allCandidatesMinusCorrectOnes.size, 5)
          println("correct answer: " + goldCandidates.head)
          println("question: " + q.questionText)
          println("candidates: " + candidates)
          println("length of allCandidatesMinusCorrectOnes: " + allCandidatesMinusCorrectOnes.size)
          println("candidates.length: " + candidates.length)
          println("correctIndex: " + correctIndex)
          require(candidates.length == correctIndex + 1)
          val (selected, _) = textSolver.solve(q.questionText, candidates, p.context)
          val aristoScore = SolverUtils.assignCredit(selected, correctIndex, candidates.length)
          val (fpr, em) = SolverUtils.assignCreditSquadScalaVersion(candidates(selected.headOption.getOrElse(0)), goldCandidates)
          println("selected answers: " + selected)
          println(s"EM: $em  / FPR: $fpr  / aristoScore: $aristoScore")
          println("-------")
          (fpr, em, (aristoScore, candidates.length))
        }
        else {
          ((0.0, 0.0, 0.0), 0.0, (0.0, 0))
        }
      }
    }.toList.filter(_._3._2 > 2)

    val (fprList, emList, scoreAndLength) = resultLists.unzip3
    val (aristoScore, len) = scoreAndLength.unzip
    val (f1List, pList, rList) = fprList.unzip3
    val avgEM = emList.sum / emList.length
    val avgP = pList.sum / pList.length
    val avgR = rList.sum / rList.length
    val avgF = f1List.sum / f1List.length
    val avgAristoScore = aristoScore.sum / aristoScore.length
    val avgLength = len.sum.toDouble / len.length

    println("------------")
    println("Overall size: " + pList.length)
    println("EM: " + avgEM)
    println("Precision: " + avgP)
    println("Recall: " + avgR)
    println("F1: " + avgF)
    println("avgAristoScore: " + avgAristoScore)
    println("avgLength: " + avgLength)
    println(s"$avgEM\t$avgP\t$avgR\t$avgF\t$avgAristoScore\t${pList.length}")
  }

  def evaluateTextSolverOnProcessBank(reader: ProcessBankReader, textSolver: TextSolver) = {
    val qAndpPairs = reader.trainingInstances.filterNotTemporals.filterNotTrueFalse.flatMap { p => p.questions.map(q => (q, p))}
    val resultLists = qAndpPairs.zipWithIndex.map{ case ((q, p), idx) =>
      println("==================================================")
      println("Processed " + idx + " out of " + qAndpPairs.size)
//      println("Paragraph: " + p)
      val candidates = q.answers.map(_.answerText)
      val correctIndex = q.correctIdxOpt.get
//          println("correct answer: " + goldCandidates.head)
//          println("question: " + q.questionText)
//          println("candidates: " + candidates)
//          println("length of allCandidatesMinusCorrectOnes: " + allCandidatesMinusCorrectOnes.size)
//          println("candidates.length: " + candidates.length)
//          println("correctIndex: " + correctIndex)
      val (selected, _) = textSolver.solve(q.questionText, candidates, p.context)
      SolverUtils.assignCredit(selected, correctIndex, candidates.length)
    }

    val avgAristoScore = resultLists.sum / resultLists.length

    println("------------")
    println("avgAristoScore: " + avgAristoScore)
  }

  def testTheDatastes() = {
    println("omnibusTrain: " + SolverUtils.omnibusTrain.length)
    println("omnibusTest: " + SolverUtils.omnibusTest.length)
    println("omnibusDev: " + SolverUtils.omnibusDev.length)
    println("publicTrain: " + SolverUtils.publicTrain.length)
    println("publicTest: " + SolverUtils.publicTest.length)
    println("publicDev: " + SolverUtils.publicDev.length)
    println("regentsTrain: " + SolverUtils.regentsTrain.length)
  }

  def testSquadPythonEvaluationScript() = {
    println(" ------- ")
    println(SolverUtils.assignCreditSquad("chemistry earth", Seq("the chemistry of the world in champaign")))
    println(SolverUtils.assignCreditSquadScalaVersion("chemistry earth", Seq("the chemistry of the world in champaign")))
    println(" ---repetition1--- ")
    println(SolverUtils.assignCreditSquad("chemistry earth", Seq("the chemistry chemistry chemistry of the world in champaign")))
    println(SolverUtils.assignCreditSquadScalaVersion("chemistry earth", Seq("the chemistry chemistry chemistry of the world in champaign")))
    println(" ---repetition2--- ")
    println(SolverUtils.assignCreditSquad("chemistry earth earth earth", Seq("the chemistry of the world in champaign")))
    println(SolverUtils.assignCreditSquadScalaVersion("chemistry earth earth earth", Seq("the chemistry of the world in champaign")))
    println(" ---repetition3--- ")
    println(SolverUtils.assignCreditSquad("chemistry chemistry chemistry earth", Seq("the chemistry of the world in champaign")))
    println(SolverUtils.assignCreditSquadScalaVersion("chemistry chemistry chemistry earth", Seq("the chemistry of the world in champaign")))
    println(" ---punctuation--- ")
    println(SolverUtils.assignCreditSquad("chemistry ; ; . earth", Seq("the chemistry of the world in champaign")))
    println(SolverUtils.assignCreditSquadScalaVersion("chemistry ; ; . earth", Seq("the chemistry of the world in champaign")))
    println(" ---articles--- ")
    println(SolverUtils.assignCreditSquad("the chemistry earth", Seq("the chemistry of the world in champaign")))
    println(SolverUtils.assignCreditSquadScalaVersion("the chemistry earth", Seq("the chemistry of the world in champaign")))
    println(" ---whitespace--- ")
    println(SolverUtils.assignCreditSquad("         chemistry earth", Seq("the chemistry of the world in champaign")))
    println(SolverUtils.assignCreditSquadScalaVersion("         chemistry earth", Seq("the chemistry of the world in champaign")))
    println(" ---exact match--- ")
    println(SolverUtils.assignCreditSquad("the chemistry earth", Seq("chemistry earth")))
    println(SolverUtils.assignCreditSquadScalaVersion("the chemistry earth", Seq("chemistry earth")))
  }

  def testNERAnnotations() = {
    val text = "As of 2012[update] research continued in many fields. The university president, John Jenkins, described his hope that Notre Dame would become \"one of the pre–eminent research institutions in the world\" in his inaugural address. The university has many multi-disciplinary institutes devoted to research in varying fields, including the Medieval Institute, the Kellogg Institute for International Studies, the Kroc Institute for International Peace studies, and the Center for Social Concerns. Recent research includes work on family conflict and child development, genome mapping, the increasing trade deficit of the United States with China, studies in fluid mechanics, computational science and engineering, and marketing trends on the Internet. As of 2013, the university is home to the Notre Dame Global Adaptation Index which ranks countries annually based on how vulnerable they are to climate change and how prepared they are to adapt."
    val ta = annotationUtils.annotate(text)
    val nersConll = ta.getView(ViewNames.NER_CONLL).getConstituents.asScala.map(c => c.getSurfaceForm + ": " + c.getLabel ).mkString("\n")
    println(nersConll)
    println("-------------")
    val nersOntontes = ta.getView(ViewNames.NER_ONTONOTES).getConstituents.asScala.map(c => c.getSurfaceForm + ": " + c.getLabel ).mkString("\n")
    println(nersOntontes)
  }

  def testChunker() = {
    fun("My estate goes to my husband, son, daughter-in-law, and nephew.")
    fun("Notre Dame students had a showdown in 1924 with which anti-catholic group?")
    fun("He is a well-respected man.")
    fun("We knew there would be a stop-off in Singapore for refuelling.")
    fun("The house was unoccupied at the time of the break-in.")
    fun("There was a build-up of traffic on the ring road.")
    fun("The design is state-of-the-art.")
    fun("The slacker video-gamed his way through life.")
    fun("Queen Victoria throne-sat for six decades.")
    fun("I changed my diet and became a no-meater.")
    fun("No-meater is too confusing without the hyphen.")
    def fun(text: String) = {
      println("text: " + text)
      val ta = annotationUtils.annotate(text)
      val shallowParse = ta.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.map(c => c.getSurfaceForm + ": " + c.getLabel).mkString("\n")
      println(shallowParse)
    }
  }

  def testWikiDataSimilarity() = {
    def call(e1: String, e2: String) = println(s"e1: $e1 / e2: $e2 / distance: ${WikiUtils.wikiDistance(e1, e2)}")
    call("Barack_Obama", "Person")
    call("Iran", "Country")
    call("United_States", "Country")
    call("University_of_Illinois_at_Urbana–Champaign", "University")
    call("Dan Roth", "Persin")
    call("Champaign,_Illinois", "Urbana,_Illinois")
    call("United_States", "Spain")
    call("English", "Spanish")
    call("English", "Arabic")
    call("Fox", "Cat")
    call("Fox", "Animal")
  }
  /*
        Result:
  [info] e1: Barack_Obama / e2: Person / distance: 3
  [info] e1: Iran / e2: Country / distance: 1
  [info] e1: United_States / e2: Country / distance: 1
  [info] e1: University_of_Illinois_at_Urbana–Champaign / e2: University / distance: 30
  [info] e1: Dan Roth / e2: Country / distance: 3
  [info] e1: Champaign,_Illinois / e2: Urbana,_Illinois / distance: 2
  [info] e1: United_States / e2: Spain / distance: 2
  [info] e1: English / e2: Spanish / distance: 2
  [info] e1: English / e2: Arabic / distance: 30
  [info] e1: Fox / e2: Cat / distance: 30
  [info] e1: Fox / e2: Animal / distance: 30
    */


  def trainAndEvaluateSquadClassifier() = {
    import SquadClassifierUtils._
    SquadClassifierUtils.populateInstances()
//    beginClassifier.learn(50)
    //endClassifier.learn(50)
//    beginClassifier.save()
    //endClassifier.save()
//    beginClassifier.test(SquadClassifierUtils.trainInstances)
//    beginClassifier.test(SquadClassifierUtils.testInstances)
//    endClassifier.test(SquadClassifierUtils.trainInstances)
//    endClassifier.test(SquadClassifierUtils.testInstances)

//    insideClassifier.learn(10)
//    insideClassifier.save()
//    insideClassifier.test(SquadClassifierUtils.trainInstances)
//    insideClassifier.test(SquadClassifierUtils.devInstances)

    pairClassifier.learn(20)
    pairClassifier.save()
    pairClassifier.test(SquadClassifierUtils.trainInstances)
    pairClassifier.test(SquadClassifierUtils.devInstances)
  }

  def decodeClassifierResults() = {
    insideClassifier.load()
    insideClassifier.test(SquadClassifierUtils.trainInstances)
    insideClassifier.test(SquadClassifierUtils.devInstances)
    SquadClassifierUtils.decodeQuestionsWithInside(train = false)
  }

  def tuneF1InsideClassifier() = {
    insideClassifier.load()
    insideClassifier.test(SquadClassifierUtils.trainInstances)
    insideClassifier.test(SquadClassifierUtils.devInstances)
    findFMaximizingThreshold(2.0 to 4.0 by 0.2)
  }

  def evaluateTopBestAnswers() = {
    insideClassifier.load()
    insideClassifier.test(SquadClassifierUtils.trainInstances)
    insideClassifier.test(SquadClassifierUtils.devInstances)
    evaluateF1OfTopKScores(2.4, train = true, 10)
  }

  def main(args: Array[String]): Unit = {
    lazy val trainReader = new SQuADReader(Constants.squadTrainingDataFile, Some(annotationUtils.pipelineService), annotationUtils)
    lazy val devReader = new SQuADReader(Constants.squadDevDataFile, Some(annotationUtils.pipelineService), annotationUtils)
    lazy val processReader = new ProcessBankReader(Some(annotationUtils.pipelineService), annotationUtils)
    val parser = new ArgumentParser(args)
    parser.experimentType() match {
      case 1 => generateCandiateAnswers(devReader)
      case 2 => testQuantifier()
      case 3 => testPipelineAnnotation()
      case 4 => testRemoteSolverWithSampleQuestion()
      case 5 => evaluateDataSetWithRemoteSolver(devReader, "salience")
      case 6 => solveSampleQuestionWithTextILP()
      case 7 => testAlignmentScore()
      case 8 => testElasticSearchSnippetExtraction()
      case 9 => testTheDatastes()
      case 10 =>
        evaluateTextSolverOnRegents(SolverUtils.publicTrain, luceneSolver)
        evaluateTextSolverOnRegents(SolverUtils.publicTest, luceneSolver)
        evaluateTextSolverOnRegents(SolverUtils.publicTrain, salienceSolver)
        evaluateTextSolverOnRegents(SolverUtils.publicTest, salienceSolver)
      case 11 =>
//        evaluateTextilpOnRegents(SolverUtils.publicTrain)
//        println("==== public train ")
//        evaluateTextilpOnRegents(SolverUtils.publicDev)
//        println("==== public dev ")
//        evaluateTextilpOnRegents(SolverUtils.publicTest)
//        println("==== public test ")
        evaluateTextSolverOnRegents(SolverUtils.regentsTrain, textILPSolver)
//        println("==== regents train  ")
      case 12 => extractKnowledgeSnippet()
      case 13 => testSquadPythonEvaluationScript()
      case 14 =>
        // evaluate any of the text solvers on the squad dataset
        evaluateTextSolverOnSquad(trainReader, luceneSolver)
      case 15 => dumpSQuADQuestionsOnDisk(devReader)
      case 16 => testCuratorAnnotation()
      case 17 => annotationUtils.processSQuADWithWikifier(trainReader)
      case 18 =>
        // puts the result of the Wikifier in redis
        annotationUtils.processSQuADWithWikifierAndPutRedis(trainReader)
      case 19 =>
        annotationUtils.verifyWikifierAnnotationsOnDisk(trainReader)
      case 20 =>
        //val question = trainReader.instances.head.paragraphs.head.questions.head
        //println(trainReader.instances(1).paragraphs.head.questions.map(_.questionText).mkString("\n"))
        val qAndpPairs = trainReader.instances.slice(0, 2).flatMap { i => i.paragraphs.flatMap{p => p.questions.map(_ -> p)}}

//        import scala.collection.JavaConverters._
//        qAndpPairs.groupBy(_._1.qTAOpt.get.getView(ViewNames.TOKENS).asScala.head.getSurfaceForm).foreach{ case (str, qs) =>
//          println("Str = " + str)
//          qs.zipWithIndex.foreach{case ((q, p), idx) =>
//            if(q.questionText.toLowerCase.contains("which ")) {
//              println("---------" + idx + "---------")
//              annotationUtils.getTargetPhrase(q, p)
//              println("gold: " + q.answers)
//            }
//          }
//        }
        qAndpPairs.zipWithIndex.foreach{ case ((q, p), idx) =>
            if(q.questionText.toLowerCase.contains("why ")) {
              println("---------" + idx + "---------")
              CandidateGeneration.getTargetPhrase(q, p)
              println("gold: " + q.answers)
            }
          }
      case 21 => println(WikiUtils.extractCategoryOfWikipageRecursively(Seq("Saint Louis University"), 5))
      case 22 => println(WikiUtils.getWikiDataId("University"))
      case 23 => println(WikiUtils.wikiAskQuery("Saint Louis University", "University", WikiDataProperties.instanceOf, 3))
      case 24 => println(WikiUtils.wikiAskQuery("Dan Roth", WikiDataProperties.person, WikiDataProperties.instanceOf, 3))
      //WikiUtils.getWikibaseCandidatesForQuestion()
//        println(WikiUtils.extractCategoryOfWikipage("Blue"))
//        println(WikiUtils.extractRelevantCategories("Color"))
      case 25 => testNERAnnotations()
      case 26 => testChunker()
      case 27 =>
        // evaluate the candidate generation recall
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0,5).flatMap{p => p.questions.slice(0, 10).map(q => (q, p))}}.take(1000)
        val (pre, rec, candSize) = qAndpPairs.zipWithIndex.map{ case ((q, p), idx) =>
//          val candidates = annotationUtils.getTargetPhrase(q, p).toSet
          //val candidates = annotationUtils.candidateGenerationWithQuestionTypeClassification(q, p)
          val candidates = CandidateGeneration.getTargetPhrase(q, p).toSet
          println("candidates = " + candidates)
          val goldCandidates = q.answers.map(_.answerText).toSet
          val pre = if (goldCandidates.intersect(candidates).nonEmpty) 1.0 else 0.0
          val rec = if(candidates.nonEmpty) 1.0 else 0.0
          (pre, rec, candidates.size)
        }.unzip3
        val avgP = pre.sum / pre.length
        val avgR = rec.sum / rec.length
        val avgCandidateLength = candSize.sum.toDouble / candSize.length
        println("Overall size: " + pre.length)
        println("Precision: " + avgP)
        println("Recall: " + avgR)
        println("AvgResult: " + avgCandidateLength)
        println("Ratio of answers with length 1: " + candSize.count(_ == 1))
        println("F1: " + 2 * avgR * avgP  / (avgP + avgR))
        println(candSize)
      case 28 =>
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap{p => p.questions.map(_ -> p)}}
        qAndpPairs.zipWithIndex.foreach{ case ((q, p), idx) =>
          println("---------" + idx + "---------")
          CandidateGeneration.candidateGenerationWithQuestionTypeClassification(q, p)
          println("gold: " + q.answers)
        }
      case 29 => testWikiDataSimilarity()
      case 30 => trainAndEvaluateSquadClassifier()
      case 31 => decodeClassifierResults()
      case 32 => tuneF1InsideClassifier()
      case 33 => evaluateTopBestAnswers()
      case 34 =>
        // evaluate the candidate generation recall
        //val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(5,10).flatMap{p => p.questions.slice(0, 10).map(q => (q, p))}}.take(25)
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap{p => p.questions.map(q => (q, p))}}
        val pairsGrouped = qAndpPairs.groupBy(_._2)
        val resultLists = pairsGrouped.zipWithIndex.flatMap{ case ((p, pairs), idx) =>
          val allCandidates = CandidateGeneration.getCandidateAnswer(p.contextTAOpt.get)
          println("==================================================")
          println("Processed " + idx + " out of " + pairsGrouped.size)
          println("Paragraph: " + p.context)
//          println("candidates = " + candidates)
            pairs.map { case (q, _) =>
              val topSenIds = SolverUtils.getSentenceScores(p, q).take(1).map(_._1)
              val topSentences = topSenIds.map(p.contextTAOpt.get.getSentence(_)).mkString(" ")
              val candidates = allCandidates.filter(topSentences.contains(_))
              println("candidates = " + candidates)
//            val ruleBased = CandidateGeneration.getTargetPhrase(q, p).toSet
            // val candidates = CandidateGeneration.candidateGenerationWithQuestionTypeClassification(q, p).toSet
//            val candidates = ruleBased
//            println("candidates = " + candidates)
            val goldCandidates = q.answers.map(_.answerText)
            val (fpr, em) = candidates.map { c => SolverUtils.assignCreditSquadScalaVersion(c, goldCandidates) }.unzip
            //val (fprRuleBased, emRuleBased) = ruleBased.map { c => SolverUtils.assignCreditSquadScalaVersion(c, goldCandidates) }.unzip
            val bestEM = (Seq(0.0) ++ em).max
            val bestFPR = (Seq((0.0, 0.0, 0.0)) ++ fpr).maxBy(_._1)
            //val bestFPRRuleBased = (Seq((0.0, 0.0, 0.0)) ++ fprRuleBased).maxBy(_._1)
//            println(s"EM: $bestEM  / bestFPR: $bestFPR ")
            //if (bestFPR._3 > 0.0 && (ruleBased.isEmpty || bestFPRRuleBased._3 == 0.0)) {
/*            if (bestEM == 0.0 && candidates.nonEmpty) {
              println("question: " + q.questionText)
              println("correct answer: ")
//              println("rule-based" + ruleBased)
              println("candidates" + candidates)
              println("Gold: " + q.answers)
              println("bestFPR: " + bestFPR) // + "  /  bestFPRRuleBased: " + bestFPRRuleBased)
              println("-------")
            }*/
            (bestFPR, bestEM, candidates.size)
          }
        }.toList

        val (fprList, emList, candidateSize) = resultLists.unzip3
        val (fprListNonEmpty, emListNonEmpty, candidateSizeNonEmpty) = resultLists.filter(_._3 > 0 ).unzip3

        val (f1List, pList, rList) = fprList.unzip3
        val avgEM = emList.sum / emList.length
        val avgP = pList.sum / pList.length
        val avgR = rList.sum / rList.length
        val avgF = f1List.sum / f1List.length
        val avgCandidateLength = candidateSize.sum.toDouble / candidateSize.length

        val (f1ListNonEmpty, pListNonEmpty, rListNonEmpty) = fprListNonEmpty.unzip3
        val avgEMNonEmpty = emList.sum / emListNonEmpty.length
        val avgPNonEmpty = pListNonEmpty.sum / pListNonEmpty.length
        val avgRNonEmpty = rListNonEmpty.sum / rListNonEmpty.length
        val avgFNonEmpty = f1ListNonEmpty.sum / f1ListNonEmpty.length
        val avgCandidateLengthNonEmpty = candidateSizeNonEmpty.sum.toDouble / candidateSizeNonEmpty.length

        println("------------")
        println("Overall size: " + pList.length)
        println("EM: " + avgEM)
        println("Precision: " + avgP)
        println("Recall: " + avgR)
        println("F1: " + avgF)
        println("avgCandidateLength: " + avgCandidateLength)
        println("Ratio of answers with length 1: " + candidateSize.count(_ == 1))
        println(s"$avgEM\t$avgP\t$avgR\t$avgF\t$avgCandidateLength\t${candidateSize.count(_ == 1)}\t${pList.length}")
        println("------nonEmpty------")
        println("Overall size: " + pListNonEmpty.length)
        println("EM: " + avgEMNonEmpty)
        println("Precision: " + avgPNonEmpty)
        println("Recall: " + avgRNonEmpty)
        println("F1: " + avgFNonEmpty)
        println("avgCandidateLength: " + avgCandidateLengthNonEmpty)
        println("Count of answers with length 1: " + candidateSizeNonEmpty.count(_ == 1))
        println(s"$avgEMNonEmpty\t$avgPNonEmpty\t$avgRNonEmpty\t$avgFNonEmpty\t$avgCandidateLengthNonEmpty\t${candidateSizeNonEmpty.count(_ == 1)}\t${pListNonEmpty.length}")
      case 35 =>
        // evaluate the candidate generation recall
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0,5).flatMap{p => p.questions.slice(0, 10).map(q => (q, p))}}.take(1000)
        val pairsGrouped = qAndpPairs.groupBy(_._2)
        val (pre, rec, candSize) = pairsGrouped.zipWithIndex.flatMap{ case ((p, pairs), idx) =>
          val candidates = CandidateGeneration.getCandidateAnswer(p.contextTAOpt.get)
          println("Processed " + idx + " out of " + pairsGrouped.size)
          println("candidates = " + candidates)
          pairs.map{ case (q, _) =>
            val goldCandidates = q.answers.map(_.answerText).distinct.toSet
            val pre = if (goldCandidates.intersect(candidates).nonEmpty) 1.0 else 0.0
            val rec = if(candidates.nonEmpty) 1.0 else 0.0
            (pre, rec, candidates.size)
          }
        }.toList.unzip3
        val avgP = pre.sum / pre.length
        val avgR = rec.sum / rec.length
        val avgCandidateLength = candSize.sum.toDouble / candSize.length
        println("Overall size: " + pre.length)
        println("Precision: " + avgP)
        println("Recall: " + avgR)
        println("AvgResult: " + avgCandidateLength)
        println("Ratio of answers with length 1: " + candSize.count(_ == 1))
        println("F1: " + 2 * avgR * avgP  / (avgP + avgR))
        println(candSize)
      case 36 =>
        // try getting substrees for tree
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0,5).flatMap{p => p.questions.slice(0, 10).map(q => (q, p))}}.take(1000)
        //val stanfordParse = qAndpPairs.head._2.contextTAOpt.get.getView(ViewNames.PARSE_STANFORD)
        println(qAndpPairs.head._2.context)
        CandidateGeneration.generateCandidates(qAndpPairs.head._2.contextTAOpt.get)

      case 37 => // getting
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0,5).flatMap{p => p.questions.slice(0, 10).map(q => (q, p))}}.take(1000)
        val minAnswerLength = qAndpPairs.map{ case (q, p) =>
          q.answers.map(_.answerText.split(" ").length).max
        }
        val bbb = minAnswerLength.groupBy(identity).map{ case (a, b) => a -> b.length }.toSeq.sortBy(_._1)
        val str = bbb.map{case (a, b) => s"$a\t$b"}.mkString("\n")
        println(str)

      case 38 => // getting stopwords and answer intersections
        val qAndpPairs = devReader.instances.flatMap { i => i.paragraphs.flatMap{p => p.questions.map(q => (q, p))}}
        val answerStrings = qAndpPairs.flatMap{ case (q, p) =>
          q.answers.map(_.answerText.toLowerCase)
        }.toSet
        println(answerStrings.intersect(CandidateGeneration.stopwordsSet).mkString("\n"))

      case 39 => // finding pattersn in lists
        val list = List("NNS", "VBG", "JJ", "NNS", "IN", "NNP", "NNP")
//        val list = List("DT", "NN", "IN", "NN", "DT", "NNP", "NNP", "NN", "VBD", "IN", "NN",
//          ".", "DT", "NN", "NN", "VBD", "VBN", "IN", "CD", ".", "DT", "VBG", "VBD", "NNP", "CC", "MD", "VB", "IN", "NNP", ".")
        println(TextAnnotationPatternExtractor.findPattern(list, List("NNS", "VBG")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("NNS", "*", "VBG")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("NNS", "?", "VBG")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("NNS", "?", "JJ")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("VBG", "?", "NNS")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("JJ")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("VBG", "*", "IN")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("VBG", "*")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("Foo")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("VBG", "*", "Bar")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("NNS")))
        println(TextAnnotationPatternExtractor.findPattern(list, List("IN", "NN")))
      case 40 =>
        val ta = DummyTextAnnotationGenerator.generateAnnotatedTextAnnotation(false, 3)
        println(TextAnnotationPatternExtractor.extractPatterns(ta))
      case 41 =>
        val qAndpPairs = devReader.instances.flatMap { i => i.paragraphs.flatMap{p => p.questions.map(q => (q, p))}}
        qAndpPairs.foreach{ case (q, p) =>
          assert(q.qTAOpt.get.hasView(ViewNames.QUANTITIES), q.qTAOpt.get.text)
          assert(p.contextTAOpt.get.hasView(ViewNames.QUANTITIES), p.contextTAOpt.get.text)
        }
      case 42 =>
        // mixture of extractors
        // evaluate the candidate generation recall
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0,5).flatMap{p => p.questions.slice(0, 10).map(q => (q, p))}}.take(1000)
        val pairsGrouped = qAndpPairs.groupBy(_._2)
        val resultLists = pairsGrouped.zipWithIndex.flatMap{ case ((p, pairs), idx) =>
          val allCandidates = CandidateGeneration.getCandidateAnswer(p.contextTAOpt.get).filter(_.split(" ").length < 7)
          println("==================================================")
          println("Processed " + idx + " out of " + pairsGrouped.size)
          println("Paragraph: " + p.context)
          println("all candidates = " + allCandidates)
          pairs.map{ case (q, _) =>
            val ruleBased = CandidateGeneration.getTargetPhrase(q, p).toSet
            //              val candidates = CandidateGeneration.candidateGenerationWithQuestionTypeClassification(q, p).toSet
            println("ruleBased = " + ruleBased)
            val candidates = if(ruleBased.isEmpty) allCandidates else ruleBased
            println("candidates = " + candidates)
            val goldCandidates = q.answers.map(_.answerText)
            val (fpr, em) = candidates.map{ c => SolverUtils.assignCreditSquadScalaVersion(c, goldCandidates) }.unzip
            val bestEM = (Seq(0.0) ++ em).max
            val bestFPR = (Seq((0.0, 0.0, 0.0)) ++ fpr).maxBy(_._1)
            println(s"EM: $bestEM  / bestFPR: $bestFPR ")
            //if(bestEM == 0.0 && candidates.nonEmpty) {
              println("question: " + q.questionText)
              println("correct answer: ")
              println(q.answers)
              println("-------")
            //}
            (bestFPR, bestEM, candidates.size)
          }
        }.toList

        val (fprList, emList, candidateSize) = resultLists.unzip3
        val (fprListNonEmpty, emListNonEmpty, candidateSizeNonEmpty) = resultLists.filter(_._3 > 0 ).unzip3

        val (f1List, pList, rList) = fprList.unzip3
        val avgEM = emList.sum / emList.length
        val avgP = pList.sum / pList.length
        val avgR = rList.sum / rList.length
        val avgF = f1List.sum / f1List.length
        val avgCandidateLength = candidateSize.sum.toDouble / candidateSize.length

        val (f1ListNonEmpty, pListNonEmpty, rListNonEmpty) = fprListNonEmpty.unzip3
        val avgEMNonEmpty = emList.sum / emListNonEmpty.length
        val avgPNonEmpty = pListNonEmpty.sum / pListNonEmpty.length
        val avgRNonEmpty = rListNonEmpty.sum / rListNonEmpty.length
        val avgFNonEmpty = f1ListNonEmpty.sum / f1ListNonEmpty.length
        val avgCandidateLengthNonEmpty = candidateSizeNonEmpty.sum.toDouble / candidateSizeNonEmpty.length

        println("------------")
        println("Overall size: " + pList.length)
        println("EM: " + avgEM)
        println("Precision: " + avgP)
        println("Recall: " + avgR)
        println("F1: " + avgF)
        println("avgCandidateLength: " + avgCandidateLength)
        println("Ratio of answers with length 1: " + candidateSize.count(_ == 1))
        println(s"$avgEM\t$avgP\t$avgR\t$avgF\t$avgCandidateLength\t${candidateSize.count(_ == 1)}\t${pList.length}")
        println("------nonEmpty------")
        println("Overall size: " + pListNonEmpty.length)
        println("EM: " + avgEMNonEmpty)
        println("Precision: " + avgPNonEmpty)
        println("Recall: " + avgRNonEmpty)
        println("F1: " + avgFNonEmpty)
        println("avgCandidateLength: " + avgCandidateLengthNonEmpty)
        println("Ratio of answers with length 1: " + candidateSizeNonEmpty.count(_ == 1))
        println(s"$avgEMNonEmpty\t$avgPNonEmpty\t$avgRNonEmpty\t$avgFNonEmpty\t$avgCandidateLengthNonEmpty\t${candidateSizeNonEmpty.count(_ == 1)}\t${pListNonEmpty.length}")

      case 43 =>
        val strs = Seq("What was the US release date for Spectre?",
          "New York City is the biggest city in the United States since what historical date?",
          "New Amsterdam became the title of New York City in what past date?")
        strs.foreach{ str =>
          val ta = annotationUtils.annotate(str)
          println(TextAnnotationPatternExtractor.whatSthDate(ta))
        }
      case 44 =>
        // analyze distribution of the correct answer across sentences
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap{p => p.questions.map(q => (q, p))}}
        val ranks = qAndpPairs.zipWithIndex.map{ case ((q, p), idx) =>
          println("Doing " + idx + "  out of " + qAndpPairs.length)
          val charStart = q.answers.head.answerStart
          val c = p.contextTAOpt.get.getView(ViewNames.TOKENS).getConstituents.asScala.toList.filter(c => c.getStartCharOffset <= charStart + 2 && c.getEndCharOffset >= charStart + 2)
          require(c.nonEmpty)
          val goldAnswerSenId = p.contextTAOpt.get.getSentenceId(c.head)
          val sentenceIdsAndScores = SolverUtils.getSentenceScores(p, q)
//        println("sentenceIdsAndScores: " + sentenceIdsAndScores)
          val corrctSenRank = sentenceIdsAndScores.zipWithIndex.collect{case a if a._1._1 == goldAnswerSenId => a._2 }
//          println("corrctSenRank: " + corrctSenRank)
          assert(corrctSenRank.length == 1)
          corrctSenRank.head
        }
        println("avgRank: " + ranks.sum.toDouble / ranks.length)
        ranks.groupBy(identity).toList.sortBy(a => a._1).foreach{case (rank, stuff) => println(s"rank: ${rank} : percentage: ${100.0 * stuff.length.toDouble / ranks.size}")}

      case 45 =>
        // get distribution of sentence length
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap{p => p.questions.map(q => (q, p))}}
        val lenList = qAndpPairs.map{ case (_, p) =>  p.contextTAOpt.get.getNumberOfSentences }
        println("avgLength: " + lenList.sum.toDouble / lenList.length)
        lenList.groupBy(identity).toList.sortBy(a => a._1).foreach{case (rank, stuff) => println(s"sentenceLength: ${rank} : percentage: ${100.0 * stuff.length.toDouble / lenList.size}")}
      case 46 =>

//        val source = "The rain in Spain falls mainly on the plain."
//        val target = "Spain has significant rainfall."
//
//        val configFile = "config/alternativeLlmConfig.txt"
//        val llm = new LlmStringComparator( new ResourceManager( configFile )) // or, zero-argument constructor to use defaults
//        val comparison = llm.compareStrings( source, target )
//        val er = llm.determineEntailment( source, target)
//        println("comparison: " + comparison)
//        println("entailment: " + er)

      case 47 =>
        // see how the answers are distributed across sentences.
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap{p => p.questions.map(q => (q, p))}}
        val lenList = qAndpPairs.map{ case (q, p) =>
          val lemmaCons = p.contextTAOpt.get.getView(ViewNames.LEMMA).getConstituents.asScala.toList
          q.answers.map{ ans =>
            val charStart = ans.answerStart
            val c = p.contextTAOpt.get.getView(ViewNames.TOKENS).getConstituents.asScala.toList.
              filter(c => c.getStartCharOffset <= charStart + 2 && c.getEndCharOffset >= charStart + 2)
            require(c.nonEmpty)
            val goldAnswerSenId = p.contextTAOpt.get.getSentenceId(c.head)
            goldAnswerSenId
          }.toSet.size
        }
        println("avgLength: " + lenList.sum.toDouble / lenList.length)
        lenList.groupBy(identity).toList.sortBy(a => a._1).foreach{case (rank, stuff) => println(s"sentenceLength: $rank : percentage: ${100.0 * stuff.length.toDouble / lenList.size}")}
      case 48 =>
        // test td-idf
        val documentList = "some text" :: "some other text" :: "dummy test text" :: Nil
        val tfIdf = new TfIdf(documentList)
        for (document <- documentList) {
          println(tfIdf.score("text", document))
        }
      case 49 =>
        // test calculating td-idf for training data
        import System.nanoTime
        def profile[R](code: => R, t: Long = nanoTime) = (code, nanoTime - t)

        val documentList = trainReader.instances.flatMap{_.paragraphs.map{ _.context  } }
        val tfIdf = new TfIdf(documentList)
        (0 to 10).foreach{ tryCount =>
          val (result, time) = profile {
            for (document <- documentList) {
              tfIdf.score("the", document)
            }
          }
          println(s"Try $tryCount: " + time / 1e9)
        }
      case 50 =>
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
      case 51 =>
        // evaluate processBank
//        evaluateTextSolverOnProcessBank(processReader, textILPSolver)
        evaluateTextSolverOnProcessBank(processReader, slidingWindowSolver)
      case 52 =>
        // write processBank on disk as json
        import java.io._
        val pw = new PrintWriter(new File("processBank-train.json" ))
        val json = Json.toJson(processReader.trainingInstances).toString
        pw.write(json)
        pw.close()
    }
  }
}