package org.allenai.ari.solvers.textilp

import java.io.File

import edu.illinois.cs.cogcomp.McTest.MCTestBaseline
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.utilities.{DummyTextAnnotationGenerator, SerializationHelper}
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
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{Constituent, TextAnnotation}
import edu.illinois.cs.cogcomp.edison.annotators.ClauseViewGenerator
import org.allenai.ari.solvers.textilp.ResultJson._

import scala.io.Source

object ExperimentsApp {
  lazy val annotationUtils = new AnnotationUtils()
  lazy val textILPSolver = new TextILPSolver(annotationUtils, 0.4, false, 0.33)
  lazy val salienceSolver = new SalienceSolver()
  lazy val luceneSolver = new LuceneSolver()
  lazy val slidingWindowSolver = new SlidingWindowSolver()

  class ArgumentParser(args: Array[String]) extends ScallopConf(args) {
    val experimentType: ScallopOption[Int] = opt[Int]("type", descr = "Experiment type", required = true)
    verify()
  }

  def testQuantifier(): Unit = {
    val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "",
      "The first type are transposons, which move within a genome by means of a DNA intermediate. Transposons can move by a \"cut-and-paste\" mechanism, which removes the element from the original site, or by a \"copy-and-paste\" mechanism, which leaves a copy behind (Figure 21.9). Both mechanisms require an enzyme called transposase, which is generally encoded by the transposon.")
    annotationUtils.pipelineService.addView(ta, ViewNames.QUANTITIES)
    annotationUtils.pipelineService.addView(ta, ViewNames.SHALLOW_PARSE)
//    println(ta)
//    println(ta.getAvailableViews)
//    println(ta.getView(ViewNames.QUANTITIES))
//    println(ta.getView(ViewNames.QUANTITIES).getConstituents.asScala.filter(_.getLabel.contains("Date")))
//    println(ta.getView(ViewNames.QUANTITIES).getConstituents.asScala.map(c => c.getSurfaceForm -> c.getLabel))
    println(ta.getView(ViewNames.TOKENS))
    println(ta.getView(ViewNames.SHALLOW_PARSE))
  }

  def testClauseView(): Unit = {
    val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "",
      "For example, many land plants today have a vascular system for transporting materials internally and a waterproof coating of wax on their leaves that slows the loss of water to the air. ")
    annotationUtils.pipelineService.addView(ta, ViewNames.PARSE_STANFORD)
    val cvg = new ClauseViewGenerator(ViewNames.PARSE_STANFORD, "clauses")
    ta.addView(cvg)
    println(ta.getAvailableViews)
    println(ta.getView("clauses"))
    println(ta.getView("clauses").getConstituents.asScala.map(c => c.getSurfaceForm -> c.getLabel).mkString("\n"))
  }

  def testStanfordDepView(): Unit = {
    val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "",
      "For example , many land plants today have a vascular system for transporting materials internally and a waterproof coating of wax on their leaves that slows the loss of water to the air .")
    annotationUtils.pipelineService.addView(ta, ViewNames.DEPENDENCY_STANFORD)
    annotationUtils.pipelineService.addView(ta, ViewNames.POS)
    val tokens = ta.getView(ViewNames.DEPENDENCY_STANFORD).getConstituents.asScala
    val posView = ta.getView(ViewNames.POS)
    println(ta.getAvailableViews)
    println(ta.getView(ViewNames.DEPENDENCY_STANFORD))
    val verbs = Set("VB", "VBZ", "VBP")
    println(tokens.map(c => c.getSurfaceForm -> c.getLabel + "  parents: " + c.getIncomingRelations.asScala.map(cc => cc.getSource.getSurfaceForm + ", " +  cc.getSource.getLabel +  posView.getConstituentsCovering(cc.getSource).get(0).getLabel) ).mkString("\n"))
    val ands = tokens.filter{c => c.getSurfaceForm == "and" && verbs.contains(posView.getConstituentsCovering(c.getIncomingRelations.get(0).getSource).get(0).getLabel)}
    val indices = 0 +: ands.map(_.getEndSpan) :+ ta.getTokens.length
    val constitunes = indices.sliding(2).map{ list => new Constituent("", "", ta, list(0), list(1)) }
    println(constitunes.map(_.getSurfaceForm).mkString("\n"))
  }

  def testIndependentClauseViewGenerator() = {
    val sentences = Seq(
      // independent
      "Today is Thursday and the test is Friday.",
      "She wants to travel the world and see wonderful sights.",
      "All of us went to the movie, and we agreed it was enjoyable.",
      "I like this class, and it is very interesting.",
      "My professor is intelligent, and I've learned a lot from her.",
      "My grandmother refuses to go to bed early, and I'm afraid she's going to catch a bad cold.",
      "I will write a homework and will turn it in tomorrow",
      "For example, many land plants today have a vascular system for transporting materials internally and a waterproof coating of wax on their leaves that slows the loss of water to the air. Early signs of these adaptations were present 420 million years ago, at which time small plants (about 10 cm high) existed that had a vascular system but lacked true roots or leaves.",
      // not independent
      "I enjoy sitting by the fireplace and reading.",
      "Hiking and biking are my favorite summertime activities.",
      "I like black and red sweatshirts"
    )
    val generator = new IndependentClauseViewGenerator("Generator")
    sentences.foreach{ s =>
      val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "", s)
      annotationUtils.pipelineService.addView(ta, ViewNames.DEPENDENCY_STANFORD)
      annotationUtils.pipelineService.addView(ta, ViewNames.POS)
      ta.addView(generator)
      println(ta.getAvailableViews)
      println(ta.getView("Generator"))
    }
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

  def evaluateTextSolverOnProcessBank(list: List[Paragraph], textSolver: TextSolver) = {
    val qAndpPairs = list.flatMap { p => p.questions.map(q => (q, p))}
    val (resultLists, confidences) = qAndpPairs.zipWithIndex.map{ case ((q, p), idx) =>
//      println("==================================================")
//      println("Processed " + idx + " out of " + qAndpPairs.size)
//      println("Paragraph: " + p)
      val candidates = q.answers.map(_.answerText)
      val correctIndex = q.correctIdxOpt.get
//          println("correct answer: " + goldCandidates.head)
//          println("question: " + q.questionText)
//          println("candidates: " + candidates)
//          println("length of allCandidatesMinusCorrectOnes: " + allCandidatesMinusCorrectOnes.size)
//          println("candidates.length: " + candidates.length)
//          println("correctIndex: " + correctIndex)
      val (selected, explanation) = textSolver.solve(q.questionText, candidates, p.context)
      val correctLabel = q.answers(correctIndex).answerText
      val score = SolverUtils.assignCredit(selected, correctIndex, candidates.length)
//      if(score < 0.5) println(" >>>>>>> Incorrect :" + score)
      score -> (explanation.confidence -> correctLabel)
    }.unzip

    val avgAristoScore = resultLists.sum / resultLists.length
//    println(confidences.mkString("\n"))

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
    lazy val processReader = new ProcessBankReader(false, annotationUtils)
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
        val qAndpPairs = trainReader.instances.slice(0, 2).flatMap { i => i.paragraphs.flatMap { p => p.questions.map(_ -> p) } }

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
        qAndpPairs.zipWithIndex.foreach { case ((q, p), idx) =>
          if (q.questionText.toLowerCase.contains("why ")) {
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
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0, 5).flatMap { p => p.questions.slice(0, 10).map(q => (q, p)) } }.take(1000)
        val (pre, rec, candSize) = qAndpPairs.zipWithIndex.map { case ((q, p), idx) =>
          //          val candidates = annotationUtils.getTargetPhrase(q, p).toSet
          //val candidates = annotationUtils.candidateGenerationWithQuestionTypeClassification(q, p)
          val candidates = CandidateGeneration.getTargetPhrase(q, p).toSet
          println("candidates = " + candidates)
          val goldCandidates = q.answers.map(_.answerText).toSet
          val pre = if (goldCandidates.intersect(candidates).nonEmpty) 1.0 else 0.0
          val rec = if (candidates.nonEmpty) 1.0 else 0.0
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
        println("F1: " + 2 * avgR * avgP / (avgP + avgR))
        println(candSize)
      case 28 =>
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap { p => p.questions.map(_ -> p) } }
        qAndpPairs.zipWithIndex.foreach { case ((q, p), idx) =>
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
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } }
        val pairsGrouped = qAndpPairs.groupBy(_._2)
        val resultLists = pairsGrouped.zipWithIndex.flatMap { case ((p, pairs), idx) =>
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
        val (fprListNonEmpty, emListNonEmpty, candidateSizeNonEmpty) = resultLists.filter(_._3 > 0).unzip3

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
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0, 5).flatMap { p => p.questions.slice(0, 10).map(q => (q, p)) } }.take(1000)
        val pairsGrouped = qAndpPairs.groupBy(_._2)
        val (pre, rec, candSize) = pairsGrouped.zipWithIndex.flatMap { case ((p, pairs), idx) =>
          val candidates = CandidateGeneration.getCandidateAnswer(p.contextTAOpt.get)
          println("Processed " + idx + " out of " + pairsGrouped.size)
          println("candidates = " + candidates)
          pairs.map { case (q, _) =>
            val goldCandidates = q.answers.map(_.answerText).distinct.toSet
            val pre = if (goldCandidates.intersect(candidates).nonEmpty) 1.0 else 0.0
            val rec = if (candidates.nonEmpty) 1.0 else 0.0
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
        println("F1: " + 2 * avgR * avgP / (avgP + avgR))
        println(candSize)
      case 36 =>
        // try getting substrees for tree
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0, 5).flatMap { p => p.questions.slice(0, 10).map(q => (q, p)) } }.take(1000)
        //val stanfordParse = qAndpPairs.head._2.contextTAOpt.get.getView(ViewNames.PARSE_STANFORD)
        println(qAndpPairs.head._2.context)
        CandidateGeneration.generateCandidates(qAndpPairs.head._2.contextTAOpt.get)

      case 37 => // getting
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0, 5).flatMap { p => p.questions.slice(0, 10).map(q => (q, p)) } }.take(1000)
        val minAnswerLength = qAndpPairs.map { case (q, p) =>
          q.answers.map(_.answerText.split(" ").length).max
        }
        val bbb = minAnswerLength.groupBy(identity).map { case (a, b) => a -> b.length }.toSeq.sortBy(_._1)
        val str = bbb.map { case (a, b) => s"$a\t$b" }.mkString("\n")
        println(str)

      case 38 => // getting stopwords and answer intersections
        val qAndpPairs = devReader.instances.flatMap { i => i.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } }
        val answerStrings = qAndpPairs.flatMap { case (q, p) =>
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
        val qAndpPairs = devReader.instances.flatMap { i => i.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } }
        qAndpPairs.foreach { case (q, p) =>
          assert(q.qTAOpt.get.hasView(ViewNames.QUANTITIES), q.qTAOpt.get.text)
          assert(p.contextTAOpt.get.hasView(ViewNames.QUANTITIES), p.contextTAOpt.get.text)
        }
      case 42 =>
        // mixture of extractors
        // evaluate the candidate generation recall
        val qAndpPairs = trainReader.instances.slice(0, 30).flatMap { i => i.paragraphs.slice(0, 5).flatMap { p => p.questions.slice(0, 10).map(q => (q, p)) } }.take(1000)
        val pairsGrouped = qAndpPairs.groupBy(_._2)
        val resultLists = pairsGrouped.zipWithIndex.flatMap { case ((p, pairs), idx) =>
          val allCandidates = CandidateGeneration.getCandidateAnswer(p.contextTAOpt.get).filter(_.split(" ").length < 7)
          println("==================================================")
          println("Processed " + idx + " out of " + pairsGrouped.size)
          println("Paragraph: " + p.context)
          println("all candidates = " + allCandidates)
          pairs.map { case (q, _) =>
            val ruleBased = CandidateGeneration.getTargetPhrase(q, p).toSet
            //              val candidates = CandidateGeneration.candidateGenerationWithQuestionTypeClassification(q, p).toSet
            println("ruleBased = " + ruleBased)
            val candidates = if (ruleBased.isEmpty) allCandidates else ruleBased
            println("candidates = " + candidates)
            val goldCandidates = q.answers.map(_.answerText)
            val (fpr, em) = candidates.map { c => SolverUtils.assignCreditSquadScalaVersion(c, goldCandidates) }.unzip
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
        val (fprListNonEmpty, emListNonEmpty, candidateSizeNonEmpty) = resultLists.filter(_._3 > 0).unzip3

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
        strs.foreach { str =>
          val ta = annotationUtils.annotate(str)
          println(TextAnnotationPatternExtractor.whatSthDate(ta))
        }
      case 44 =>
        // analyze distribution of the correct answer across sentences
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } }
        val ranks = qAndpPairs.zipWithIndex.map { case ((q, p), idx) =>
          println("Doing " + idx + "  out of " + qAndpPairs.length)
          val charStart = q.answers.head.answerStart
          val c = p.contextTAOpt.get.getView(ViewNames.TOKENS).getConstituents.asScala.toList.filter(c => c.getStartCharOffset <= charStart + 2 && c.getEndCharOffset >= charStart + 2)
          require(c.nonEmpty)
          val goldAnswerSenId = p.contextTAOpt.get.getSentenceId(c.head)
          val sentenceIdsAndScores = SolverUtils.getSentenceScores(p, q)
          //        println("sentenceIdsAndScores: " + sentenceIdsAndScores)
          val corrctSenRank = sentenceIdsAndScores.zipWithIndex.collect { case a if a._1._1 == goldAnswerSenId => a._2 }
          //          println("corrctSenRank: " + corrctSenRank)
          assert(corrctSenRank.length == 1)
          corrctSenRank.head
        }
        println("avgRank: " + ranks.sum.toDouble / ranks.length)
        ranks.groupBy(identity).toList.sortBy(a => a._1).foreach { case (rank, stuff) => println(s"rank: ${rank} : percentage: ${100.0 * stuff.length.toDouble / ranks.size}") }

      case 45 =>
        // get distribution of sentence length
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } }
        val lenList = qAndpPairs.map { case (_, p) => p.contextTAOpt.get.getNumberOfSentences }
        println("avgLength: " + lenList.sum.toDouble / lenList.length)
        lenList.groupBy(identity).toList.sortBy(a => a._1).foreach { case (rank, stuff) => println(s"sentenceLength: ${rank} : percentage: ${100.0 * stuff.length.toDouble / lenList.size}") }
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
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } }
        val lenList = qAndpPairs.map { case (q, p) =>
          val lemmaCons = p.contextTAOpt.get.getView(ViewNames.LEMMA).getConstituents.asScala.toList
          q.answers.map { ans =>
            val charStart = ans.answerStart
            val c = p.contextTAOpt.get.getView(ViewNames.TOKENS).getConstituents.asScala.toList.
              filter(c => c.getStartCharOffset <= charStart + 2 && c.getEndCharOffset >= charStart + 2)
            require(c.nonEmpty)
            val goldAnswerSenId = p.contextTAOpt.get.getSentenceId(c.head)
            goldAnswerSenId
          }.toSet.size
        }
        println("avgLength: " + lenList.sum.toDouble / lenList.length)
        lenList.groupBy(identity).toList.sortBy(a => a._1).foreach { case (rank, stuff) => println(s"sentenceLength: $rank : percentage: ${100.0 * stuff.length.toDouble / lenList.size}") }
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

        val documentList = trainReader.instances.flatMap {
          _.paragraphs.map {
            _.context
          }
        }
        val tfIdf = new TfIdf(documentList)
        (0 to 10).foreach { tryCount =>
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
        //        evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterTemporals, textILPSolver)
        //        println("filterTemporals: ")

        //        evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterCauseQuestions, textILPSolver)
        //        println("filterCauseQuestions: ")


        //        evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterCResultQuestions, textILPSolver)
        //        println("filterCResultQuestions: ")

       //  evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterWhatDoesItDo, textILPSolver)
       //  println("filterWhatDoesItDo: ")

      evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals, textILPSolver)
      println("no-temporals/no true or false: ")

//        evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals.filterNotWhatDoesItDo.filterNotCResultQuestions, textILPSolver)
//        println("no-temporals/no true or false: ")

//              (0.3 to 0.5 by 0.02).foreach{ weight =>
//                lazy val solver = new TextILPSolver(annotationUtils, 0.4, verbose = false, weight)
//                evaluateTextSolverOnProcessBank(processReader.trainingInstances.filterNotTrueFalse.filterNotTemporals, solver)
//                println("no-temporals/no true or false/weight: " + weight)
//              }
      // evaluateTextSolverOnProcessBank(processReader, slidingWindowSolver)
      case 52 =>
        // write processBank on disk as json
        import java.io._
        val pw = new PrintWriter(new File("processBank-train2.json"))
        val json = Json.toJson(processReader.trainingInstances).toString
        pw.write(json)
        pw.close()

      case 53 =>
        val list = List(
          (4.3999999999999995, "False"),
          (2.7, "False"),
          (3.4750764029270083, "False"),
          (5.218857060304387, "False"),
          (1.0477330385949053, "False"),
          (4.5930571385433705, "False"),
          (3.3437923184743807, "True"),
          (2.7, "False"),
          (3.6, "True"),
          (2.809181013882808, "True"),
          (5.648828749431884, "True"),
          (3.4526406493279973, "False"),
          (1.7395776316755533, "False"),
          (3.7264931111166786, "False"),
          (1.2893678361840508, "False"),
          (4.5, "True"),
          (5.9301358980358305, "False"),
          (2.2, "False"),
          (3.485991220138836, "True"),
          (3.2444444444444445, "True"),
          (0.9250000000000002, "True"),
          (3.15, "True"),
          (4.2731863259819995, "False"),
          (2.151320914317883, "True"),
          (8.005681533783543, "True"),
          (3.939583333333333, "False"),
          (1.7999999999999998, "True"),
          (5.9156939484738835, "False"),
          (4.310374341315811, "True"),
          (4.761922173960414, "False"),
          (3.182963620204129, "False"),
          (4.2629413735429855, "False"),
          (1.8, "False"),
          (5.074873212779324, "False"),
          (3.870689655172414, "False"),
          (4.9723028813491235, "True"),
          (2.1213764230460206, "False"),
          (2.920169715368538, "True"),
          (2.9021705380360587, "False"),
          (3.79074300885396, "True"),
          (3.4870458569613514, "False"),
          (2.7, "False"),
          (1.5951587516273693, "True"),
          (6.571601186200417, "False"),
          (3.5839640474975885, "True"),
          (3.3366683073589316, "False"),
          (2.4577460878843156, "False"),
          (3.104407228784471, "True"),
          (5.015050279316102, "True"),
          (2.49837698649533, "True"),
          (1.9158550613496934, "True"),
          (3.1539576632660427, "True"),
          (4.363674432492519, "False"),
          (5.235964520047501, "False"),
          (7.222962975312836, "False"),
          (2.0124976962260863, "False"),
          (3.6338982139450016, "False"),
          (3.6000000000000005, "False"),
          (3.9948310926592665, "False"),
          (4.1634212665934625, "False"),
          (4.1634212665934625, "False"),
          (3.1674055928451006, "True"),
          (5.763105834435255, "True"),
          (2.4392366532968626, "True"),
          (5.891555633058751, "True"),
          (3.287884160169278, "False"),
          (3.5604433062795406, "True"),
          (3.209980057902207, "False"),
          (4.238858515254964, "False"),
          (2.94196542911558, "True"),
          (4.9, "True"),
          (2.441374994683572, "False"),
          (2.5476282748245622, "True"),
          (3.6, "False"),
          (6.476023508004646, "True"),
          (4.250692787083008, "False"),
          (2.7, "False"),
          (2.719254816038015, "False"),
          (7.922111564439183, "True"),
          (3.210449039289768, "True"),
          (4.702574658357156, "True"),
          (4.908333333333333, "False"),
          (1.6925790485359795, "True"),
          (3.587123462131733, "False"),
          (2.1560216095557894, "True"),
          (5.696917807792232, "False")
        )

        (2.2 to 10.5 by 0.05).foreach { th =>
          val (pTrue, pFalse) = list.partition(_._1 > th)
          val accuracy = (pTrue.count(_._2 == "True") + pFalse.count(_._2 == "False")).toDouble / list.length
          println("Th: " + th + "  / acc: " + accuracy)
        }

      case 54 =>
        // extract buggy squad examples:
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap { p => p.questions.map(q => (q, p)) } }
        qAndpPairs.foreach { case (q, p) =>
          val problematic = q.answers.exists { ans =>
            p.context.contains(s" ${ans.answerText} ")
          }
          if (problematic) {
            println("annp: " + p.context)
            println("q: " + q.questionText)
            println("a: " + q.answers)
          }
        }

      case 55 =>
        // write the bioProcess questions on disk, as well as their predictions
        import java.io._
        val pw = new PrintWriter(new File("processBank-train.tsv"))
        val qAndpPairs = processReader.trainingInstances.flatMap { p => p.questions.map(q => (q, p)) }
        qAndpPairs.zipWithIndex.foreach { case ((q, p), idx) =>
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
      case 56 => testClauseView()
      case 57 => testStanfordDepView()
      case 58 => testIndependentClauseViewGenerator()
      case 59 =>
        // answerText: String, answerStart: Int, aTAOpt: Option[TextAnnotation] = None
        val q = Question("", "", Seq(Answer("AnsText", 1, None)), None, correctIdxOpt = Some(1))
        val listOfQs = List(q, q, q)
        val listOfP = List(Paragraph("par1", listOfQs, None), Paragraph("par1", listOfQs, None))
        val json = Json.toJson(listOfP).toString
        println(json)
      case 60 =>
        // test extarction of the constituent after "without "
        val ta = annotationUtils.pipelineService.createAnnotatedTextAnnotation("", "", "What happens without Donuld Trump?")
        val toks = ta.getView(ViewNames.TOKENS).getConstituents.asScala
        val withoutTok = toks.filter(_.getSurfaceForm == "without").head
        val after = toks.filter(c => c.getStartSpan >= withoutTok.getStartSpan).minBy(_.getStartSpan)
        println(after)
      case 61 =>
        def experiment(k: Int, j: Int) = {
          var inHowManyCasesThatOneReasonAppearsBeforeQuestionTheCorrectAnsTheLatter = 0
          var inHowManyCasesThatOneReasonAppearsBeforeQuestion = 0
          // analyze the result questions
          def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
            pCons.map { c =>
              TextILPSolver.getMaxScore(qCons, Seq(c))
            }.zipWithIndex.maxBy(_._1)._2
          }
          val paragraphs = processReader.trainingInstances.filterCResultQuestions
          val list = paragraphs.flatMap { p =>
            val pCons = p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
            p.questions.map { q =>
              val qCons = q.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
              val qIdx = getClosestIndex(qCons, pCons)
              val ans1Cons = q.answers(0).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
              val ans2Cons = q.answers(1).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
              val a1Idx = getClosestIndex(ans1Cons, pCons)
              val a2Idx = getClosestIndex(ans2Cons, pCons)
              if (a1Idx < qIdx - k && a2Idx > qIdx + j) {
                // at least one of the answers happens before the question
                inHowManyCasesThatOneReasonAppearsBeforeQuestion += 1
                if (q.correctIdxOpt.get == 1) {
                  inHowManyCasesThatOneReasonAppearsBeforeQuestionTheCorrectAnsTheLatter += 1
                }
              }
              if (a2Idx < qIdx - k && a1Idx > qIdx + j) {
                inHowManyCasesThatOneReasonAppearsBeforeQuestion += 1
                if (q.correctIdxOpt.get == 0) {
                  inHowManyCasesThatOneReasonAppearsBeforeQuestionTheCorrectAnsTheLatter += 1
                }
              }
              val doesCorrectIdxAppearsLater = if ((q.correctIdxOpt.get == 0 && a1Idx >= a2Idx) || (q.correctIdxOpt.get == 1 && a1Idx <= a2Idx)) 1 else 0
              //println(q.questionText)
              //println(s"$a1Idx\t$a2Idx\t${q.correctIdxOpt.get}\t$doesCorrectIdxAppearsLater\t${2*(a2Idx - a1Idx)*(q.correctIdxOpt.get - 0.5)}\t${qIdx}\n")
              doesCorrectIdxAppearsLater
            }
          }
          println("k: " + k + " / j: " + j)
          val ratio1 = inHowManyCasesThatOneReasonAppearsBeforeQuestionTheCorrectAnsTheLatter.toDouble / inHowManyCasesThatOneReasonAppearsBeforeQuestion
          val ratio2 = list.sum.toDouble / list.length
          println("list.length: " + list.length)
          println("ratio1: " + ratio1)
          println("ratio2: " + ratio2)
          println("inHowManyCasesThatOneReasonAppearsBeforeQuestion: " + inHowManyCasesThatOneReasonAppearsBeforeQuestion)
          println("list.length: " + list.length)
          println("-----------------------")
        }

        for {
          k <- 0 to 7 by 1
          j <- 0 to 10 by 1
        }
          experiment(k, j)

      // a few good selections
      // k: 0 / j: 5	0.8	30
      // k: 6 / j: 0	0.75	32

      case 62 =>
        def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
          pCons.map { c =>
            println("c: " + c)
            val score = TextILPSolver.getMaxScore(qCons, Seq(c))
            println("score: " + score)
            score
          }.zipWithIndex.maxBy(_._1)._2
        }
        // test extarction of the constituent after "without "
        val pTA = annotationUtils.pipelineService.createAnnotatedTextAnnotation("", "",
          "After gametes fuse and form a diploid zygote, meiosis occurs without a multicellular diploid offspring developing. Meiosis produces not gametes but haploid cells that then divide by mitosis and give rise to either unicellular descendants or a haploid multicellular adult organism. Subsequently, the haploid organism carries out further mitoses, producing the cells that develop into gametes.")
        val aTA = annotationUtils.pipelineService.createAnnotatedTextAnnotation("", "", "Haploid cells would not be produced")
        val pCons = pTA.getView(ViewNames.SHALLOW_PARSE).asScala.toList
        val ans1Cons = aTA.getView(ViewNames.TOKENS).asScala.toList
        val a1Idx = getClosestIndex(ans1Cons, pCons)
        println("a1Idx: " + a1Idx)
      case 63 =>
        // processes the bio-Process data with curator coref
        val separator = "<><><><>"
        import java.io._
        val pw = new PrintWriter(new File("bioProcessCoref.txt"))
        val list = processReader.trainingInstances ++ processReader.testInstances
        list.zipWithIndex.foreach { case (p, idx) =>
          println(" ===  processed " + idx + " out of  " + list.size)
          val ta = annotationUtils.annotateWithCuratorCorefAndCache(p.context)
          //          annotationUtils.curatorService.addView(ta, ViewNames.COREF_HEAD)
          //          annotationUtils.curatorService.addView(ta, ViewNames.COREF_EXTENT)
          val json = SerializationHelper.serializeToJson(ta)
          pw.write(json + separator)
          println(ta.getAvailableViews)
        }
        pw.close()
      case 64 =>
        // reading the saved coref-textAnnotations from disk
        val separator = "<><><><>"
        // reads in the bio-process data
        val lines = Source.fromFile(new File("bioProcessCoref.txt")).getLines().mkString.split(separator)
        lines.foreach { l =>
          try {
            val ta = SerializationHelper.deserializeFromJson(l)
            println(ta.getAvailableViews)
          }
          catch {
            case e: Exception =>
              println("catching the excepton . . . ")
              println(l)
          }
        }
      case 65 =>
        val list = processReader.trainingInstances ++ processReader.testInstances
        val ta = annotationUtils.bioProcessCorefMap.apply(list.head.context)

      case 66 =>
        val paragraph = "Second, the presence of X-gal in the medium allows us to distinguish colonies with recombinant plasmids from those with nonrecombinant plasmids. Colonies containing nonrecombinant plasmids have the lacZ gene intact and will produce functional beta-galactosidase. These colonies will be blue because the enzyme hydrolyzes the X-gal in the medium, forming a blue product. In contrast, no functional beta-galactosidase is produced in colonies containing recombinant plasmids with foreign DNA inserted into the lacZ gene; these colonies will therefore be white."
        val ta = annotationUtils.bioProcessCorefMap.apply(paragraph)
        val cons = ta.getView(ViewNames.COREF).getConstituents.asScala
        cons.foreach { c =>
          println("c: " + c + " " + c.getLabel + " / " + c.getIncomingRelations.asScala.foreach { e => e.getSource } + " / " + c.getOutgoingRelations.asScala.foreach { e => e.getTarget })
        }

        for {
          sen1 <- -0 until ta.getNumberOfSentences
          sen2 <- -0 until ta.getNumberOfSentences
        } {
          println(s"Sen1: $sen1 - Sen2: $sen2 - " + twoSentencesAreCoreferred(ta, sen1, sen2))
        }

        def twoSentencesAreCoreferred(ta: TextAnnotation, sen1: Int, sen2: Int): Int = {
          if (sen1 != sen2) {
            val toks = ta.getView(ViewNames.COREF).asScala
            val toksInSen1 = toks.filter(_.getSentenceId == sen1)
            val toksInSen2 = toks.filter(_.getSentenceId == sen2)
            toksInSen1.map(_.getLabel).toSet.intersect(toksInSen2.map(_.getLabel).toSet).size
          }
          else {
            0
          }
        }
      case 67 =>
        var a = 0
        var b = 0
        // analyze the result questions
        def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
          pCons.map { c =>
            TextILPSolver.getMaxScore(qCons, Seq(c))
          }.zipWithIndex.maxBy(_._1)._2
        }
        val paragraphs = processReader.trainingInstances.filterCauseQuestions
        val list = paragraphs.flatMap { p =>
          val pCons = p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
          p.questions.map { q =>
            val qCons = q.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
            val qIdx = getClosestIndex(qCons, pCons)
            val ans1Cons = q.answers(0).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
            val ans2Cons = q.answers(1).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
            val a1Idx = getClosestIndex(ans1Cons, pCons)
            val a2Idx = getClosestIndex(ans2Cons, pCons)
            /*            if(a1Idx < qIdx || a2Idx < qIdx) {
              // at least one of the answers happens before the question
              a += 1
              if (a1Idx < qIdx && q.correctIdxOpt.get == 0) {
                b += 1
              }
              if (a2Idx < qIdx && q.correctIdxOpt.get == 1) {
                b += 1
              }
            }*/

            /*              // at least one of the answers happens before the question
              if (a1Idx < qIdx && a2Idx > qIdx ) {
                a += 1
                if(q.correctIdxOpt.get == 0) b += 1
              }
              if (a2Idx < qIdx && a1Idx > qIdx) {
                a += 1
                if(q.correctIdxOpt.get == 1) b += 1
              }*/

            // at least one of the answers happens before the question
            //            if (a1Idx < qIdx && a2Idx > qIdx ) {
            //              a += 1
            //              if(q.correctIdxOpt.get == 0) b += 1
            //            }
            //            if (a2Idx < qIdx && a1Idx > qIdx) {
            //              a += 1
            //              if(q.correctIdxOpt.get == 1) b += 1
            //            }

            /*             if((a1Idx < qIdx && a2Idx > qIdx) || (a2Idx < qIdx && a1Idx > qIdx)) { // at least one of the answers happens before the question
              a += 1
              if (a1Idx > a2Idx && q.correctIdxOpt.get == 0) {
                b += 1
              }
              if (a2Idx > a1Idx && q.correctIdxOpt.get == 1) {
                b += 1
              }
            }*/
            val doesCorrectIdxAppearsLater = if ((q.correctIdxOpt.get == 0 && a1Idx >= a2Idx) || (q.correctIdxOpt.get == 1 && a1Idx <= a2Idx)) 1 else 0
            println(q.questionText)
            println(s"$a1Idx\t$a2Idx\t${qIdx}\t${q.correctIdxOpt.get}\t$doesCorrectIdxAppearsLater\t${2 * (a1Idx - a2Idx) * (q.correctIdxOpt.get - 0.5)}\n")
            doesCorrectIdxAppearsLater
          }
        }
        val ratio1 = b.toDouble / a
        val ratio2 = list.sum.toDouble / list.length
        println("a: " + a)
        println("ratio1: " + ratio1)
        println("ratio2: " + ratio2)

      case 68 =>
        // analyze the result questions
        def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
          pCons.map { c =>
            TextILPSolver.getMaxScore(qCons, Seq(c))
          }.zipWithIndex.maxBy(_._1)._2
        }
        val paragraphs = processReader.trainingInstances.filterWhatDoesItDo
        println("\n\n\n\n\n\n")
        val list = paragraphs.filter(_.questions.nonEmpty).foreach { p =>
          println("-------------------------------")
          val pCons = p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
          println("paragraph: " + pCons.zipWithIndex.map{case (c, i) => (c.getSurfaceForm, i) }.mkString)
          p.questions.foreach { q =>
            println(">>>>>>>>")
            val qCons = q.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
            val qIdx = getClosestIndex(qCons, pCons)
            val ans1Cons = q.answers(0).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
            val ans2Cons = q.answers(1).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
            val a1Idx = getClosestIndex(ans1Cons, pCons)
            val a2Idx = getClosestIndex(ans2Cons, pCons)
            println(s"$qIdx\t$a1Idx\t$a2Idx\n")
            println("question: " + q.questionText)
            println("answers: "+ q.answers)
          }
        }

      case 69 =>
        var a1 = 0
        var a2 = 0

        var b1 = 0
        var b2 = 0

        var c1 = 0
        var c2 = 0

        // analyze the result questions
        def getClosestIndex(qCons: Seq[Constituent], pCons: Seq[Constituent]): Int = {
          pCons.map { c =>
            TextILPSolver.getAvgScore(qCons, Seq(c))
          }.zipWithIndex.maxBy(_._1)._2
        }
        val paragraphs = processReader.trainingInstances.filterWhatDoesItDo
        paragraphs.foreach { p =>
          val pCons = p.contextTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
          p.questions.foreach { q =>
            val qCons = q.qTAOpt.get.getView(ViewNames.SHALLOW_PARSE).asScala.toList
            val qIdx = getClosestIndex(qCons, pCons)
            val ans1Cons = q.answers(0).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
            val ans2Cons = q.answers(1).aTAOpt.get.getView(ViewNames.TOKENS).asScala.toList
            val a1Idx = getClosestIndex(ans1Cons, pCons)
            val a2Idx = getClosestIndex(ans2Cons, pCons)
            // one before, one after: after is the anser
            if (a1Idx < qIdx && a2Idx > qIdx) {
              // at least one of the answers happens before the question
              a1 += 1
              if (q.correctIdxOpt.get == 1) {
                a2 += 1
              }
            }
            if (a1Idx > qIdx && a2Idx < qIdx) {
              // at least one of the answers happens before the question
              a1 += 1
              if (q.correctIdxOpt.get == 0) {
                a2 += 1
              }
            }
            // both after: closer is the answer
            if (a1Idx > qIdx && a2Idx > qIdx) {
              // at least one of the answers happens before the question
              b1 += 1
              if (a2Idx < a1Idx && q.correctIdxOpt.get == 1) {
                b2 += 1
              }
              if (a2Idx > a1Idx && q.correctIdxOpt.get == 0) {
                b2 += 1
              }
            }

            // both before: closed is the answer
            if (a1Idx < qIdx && a2Idx < qIdx) {
              // at least one of the answers happens before the question
              c1 += 1
              if (a2Idx > a1Idx && q.correctIdxOpt.get == 1) {
                c2 += 1
              }
              if (a2Idx < a1Idx && q.correctIdxOpt.get == 0) {
                c2 += 1
              }
            }
          }
        }
        val ratioA = a2.toDouble / a1
        val ratioB = b2.toDouble / b1
        val ratioC = c2.toDouble / c1
        println(s"ratioA: $ratioA - a1: $a1 - a2: $a2")
        println(s"ratioB: $ratioB - b1: $b1 - b2: $b2")
        println(s"ratioC: $ratioC - c1: $c1 - c2: $c2")
        println("-----------------------")
    }
  }
}