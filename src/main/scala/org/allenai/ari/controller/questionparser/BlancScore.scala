package org.allenai.ari.controller.questionparser

import org.allenai.common.Resource
import scopt.OptionParser

private case class BlancScoreCLI(inputFilename: String = "")

object BlancScore {

  /** A toy command-line tool for computing the BlancScore of a fill-in-the-blank generator.
    *
    * format: OFF
    * Usage: BlancScore [options]
    *
    * -i <file> | --input <file>
    *     the file containing the Blanc data
    * format: ON
    *
    * @param args command-line arguments (see above)
    */
/*  def main(args: Array[String]) {
    val optionParser = new OptionParser[BlancScoreCLI]("BlancScore") {
      opt[String]('i', "input") required () valueName ("<file>") action { (x, c) =>
        c.copy(inputFilename = x)
      } text ("the file containing the Blanc data")
    }
    val clArgs: BlancScoreCLI = optionParser.parse(args, BlancScoreCLI()).get
    val fitbChallengeProblems = BlancChallengeProblem.readFromFile(clArgs.inputFilename)
    val fitbGenerator = FillInTheBlankGenerator.mostRecent

    var correctCounter: Int = 0
    for (fitbChallengeProblem <- fitbChallengeProblems) {
      val maybeFitb: Option[FillInTheBlankQuestion] =
        fitbGenerator.generateFITB(fitbChallengeProblem.question)
      maybeFitb match {
        case Some(fitb) =>
          if (fitbChallengeProblem.isCorrectAnswer(fitb)) {
            correctCounter += 1
          } else {
            println(fitbChallengeProblem.question)
            println(fitb.text)
            fitbChallengeProblem.answers foreach { x => println(s"  ${x.tokenizedString}") }
          }
        case None =>
      }
    }
    val percentCorrect: Float = correctCounter.toFloat / fitbChallengeProblems.length
    println(s"Accuracy: $percentCorrect")
  }*/
}

/** A BlancChallengeProblem consists of an interrogative question and a set of fill-in-the-blank
  * analogs. It is intended as an evaluation method for implementations of the
  * FillInTheBlankGenerator interface. A generator answers the BlancChallengeProblem through its
  * .generateFITB method, which takes a QuestionParse object and returns a FillInTheBlank question.
  * If the returned fill-in-the-blank question matches one of the acceptable answers for the
  * BlancChallengeProblem, then the FillInTheBlankGenerator has correctly answered the
  * BlancChallengeProblem.
  *
  * @param question the interrogative question
  * @param answers a set of valid fill-in-the-blank analogs
  */
case class BlancChallengeProblem(question: QuestionParse, answers: Set[FillInTheBlankQuestion]) {

  /** Returns true if the candidate answer matches one of the correct answers.
    *
    * "Matches" currently means an exact string match (after spaces and certain punctuation
    * -- commas and colons -- are removed).
    *
    * @param candidateAnswer a candidate answer to the challenge problem
    * @return true iff the candidate answer matches one of the correct answers
    */
  def isCorrectAnswer(candidateAnswer: FillInTheBlankQuestion): Boolean = {
    (answers map { x =>
      stripWhitespaceAndPunctuation(x.tokenizedString)
    }).contains(stripWhitespaceAndPunctuation(candidateAnswer.text))
  }

  private def stripWhitespaceAndPunctuation(str: String) = {
    val toRemove = ",:; ".toSet
    str.filterNot(toRemove)
  }
}

object BlancChallengeProblem {

  /** Reads a sequence of a challenge problems from a file that contains one problem per line.
    *
    * Each line is in tab-separated value (TSV) format, with the following fields:
    * -> 0: the interrogative question
    * -> 1: an "auxiliary" fill-in-the-blank question (ignored for now)
    * -> 2+: valid fill-in-the-blank analogs
    *
    * @param filename the file to read from
    * @return a sequence of BlancChallengeProblems
    */
  def readFromFile(filename: String): Seq[BlancChallengeProblem] = {
    Resource.using(scala.io.Source.fromFile(filename)) { source =>
      val problems = for {
        line <- source.getLines()
      } yield {
        val lineFields: IndexedSeq[String] = Util.readTabSeparatedValueLine(line)
        val question: QuestionParse = QuestionParse.constructFromString(lineFields(0))
        val answers: Set[FillInTheBlankQuestion] =
          lineFields.slice(2, lineFields.size).toSet map { x: String =>
            FillInTheBlankQuestion(x)
          }
        BlancChallengeProblem(question, answers)
      }
      problems.toList
    }
  }
}
