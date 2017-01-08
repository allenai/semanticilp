package org.allenai.ari.solvers.textilp

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import org.allenai.ari.solvers.squad.{SquadClassifier, SquadClassifierUtils}
import org.allenai.ari.solvers.textilp.alignment.AlignmentFunction
import org.allenai.ari.solvers.textilp.solvers.{LuceneSolver, SalienceSolver, TextILPSolver, TextSolver}
import org.allenai.ari.solvers.textilp.utils.WikiUtils.WikiDataProperties
import org.allenai.ari.solvers.textilp.utils._
import org.rogach.scallop._

import scala.collection.JavaConverters._

object ExperimentsApp {
  lazy val annotationUtils = new AnnotationUtils()
  lazy val textILPSolver = new TextILPSolver(annotationUtils)
  lazy val salienceSolver = new SalienceSolver()
  lazy val luceneSolver = new LuceneSolver()

  class ArgumentParser(args: Array[String]) extends ScallopConf(args) {
    val experimentType: ScallopOption[Int] = opt[Int]("type", descr = "Experiment type", required = true)
    verify()
  }

  def testQuantifier(): Unit = {
    val ta = annotationUtils.pipelineService.createBasicTextAnnotation("", "",
      "The annual NFL Experience was held at the Moscone Center in San Francisco. In addition, \"Super Bowl City\" opened on January 30 at Justin Herman Plaza on The Embarcadero, featuring games and activities that will highlight the Bay Area's technology, culinary creations, and cultural diversity. More than 1 million people are expected to attend the festivities in San Francisco during Super Bowl Week. San Francisco mayor Ed Lee said of the highly visible homeless presence in this area \"they are going to have to leave\". San Francisco city supervisor Jane Kim unsuccessfully lobbied for the NFL to reimburse San Francisco for city services in the amount of $5 million.")
    annotationUtils.pipelineService.addView(ta, ViewNames.QUANTITIES)
    println(ta)
    println(ta.getAvailableViews)
    println(ta.getView(ViewNames.QUANTITIES))
    println(ta.getView(ViewNames.QUANTITIES).getConstituents.asScala.filter(_.getLabel.contains("Date")))
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
              val candidateAnswers = annotationUtils.getCandidateAnswer(annotation)
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
              val candidateAnswers = annotationUtils.getCandidateAnswer(annotation).toSeq
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

  def evaluateTextilpOnSquad(reader: SQuADReader) = {
    SolverUtils.printMemoryDetails()
    val (exactMatch, f1, total) = reader.instances.slice(0, 30).zipWithIndex.flatMap {
      case (ins, idx) =>
        println("Idx: " + idx + " / ratio: " + idx * 1.0 / reader.instances.size)
        ins.paragraphs.slice(0, 5).flatMap { p =>
          p.contextTAOpt match {
            case None => throw new Exception("The instance does not contain annotation . . . ")
            case Some(annotation) =>
              val candidateAnswers = annotationUtils.getCandidateAnswer(annotation).toSeq
              p.questions.slice(0, 1).map { q =>
                val (selected, _) = textILPSolver.solve(q.questionText, candidateAnswers, p.context)
                SolverUtils.assignCreditSquad(candidateAnswers(selected.head), q.answers.map(_.answerText))
              }
          }
        }
    }.unzip3
    println("Average exact match: " + exactMatch.sum / total.sum + "  /   Average f1: " + f1.sum / total.sum)
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
    println(SolverUtils.assignCreditSquad("the country in the east", Seq("east", "world")))
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
    beginClassifier.learn(10)
    endClassifier.learn(10)
    beginClassifier.test()
    endClassifier.test()
  }

  def main(args: Array[String]): Unit = {
    lazy val trainReader = new SQuADReader(Constants.squadTrainingDataFile, Some(annotationUtils.pipelineService), annotationUtils)
    lazy val devReader = new SQuADReader(Constants.squadDevDataFile, Some(annotationUtils.pipelineService), annotationUtils)
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
      case 14 => evaluateTextilpOnSquad(trainReader)
      case 15 => dumpSQuADQuestionsOnDisk(devReader)
      case 16 => testCuratorAnnotation()
      case 17 => annotationUtils.processSQuADWithWikifier(trainReader)
      case 18 => annotationUtils.processSQuADWithWikifierAndPutRedis(devReader)
      case 19 => annotationUtils.verifyWikifierAnnotationsOnDisk(trainReader)
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
              annotationUtils.getTargetPhrase(q, p)
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
          val candidates = annotationUtils.getCandidateAnswer(p.contextTAOpt.get)
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
        println("F1: " + 2 * avgR * avgR  / (avgP + avgR))
        println(candSize)
      case 28 =>
        val qAndpPairs = trainReader.instances.flatMap { i => i.paragraphs.flatMap{p => p.questions.map(_ -> p)}}
        qAndpPairs.zipWithIndex.foreach{ case ((q, p), idx) =>
            println("---------" + idx + "---------")
            annotationUtils.candidateGenerationWithQuestionTypeClassification(q, p)
            println("gold: " + q.answers)
        }
      case 29 => testWikiDataSimilarity()
      case 30 => trainAndEvaluateSquadClassifier()
    }
  }
}