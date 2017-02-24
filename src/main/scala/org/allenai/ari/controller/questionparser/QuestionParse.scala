package org.allenai.ari.controller.questionparser

import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.polyparser.{ Parser, TransitionParser }
import org.allenai.nlpstack.tokenize.defaultTokenizer

/** A Question is an abstraction of a test question. */
sealed trait QuestionParse

object QuestionParse {

  /** A dispatcher function that constructs a Question object based on the text.
    *
    * @param text the raw question text
    * @return the initialized question
    */
  def constructFromString(text: String): QuestionParse = {
    val tokens: Seq[Token] = Parser.tokenizeSentence(text)
    if (tokens.last.word == Symbol("?")) {
      StandardQuestion(text)
    } else if ((tokens map { _.word }).contains(FillInTheBlankQuestion.blankSymbol)) {
      FillInTheBlankQuestion(text)
    } else if (tokens.last.word != Symbol(".")) {
      FillInTheBlankQuestion(s"$text ${FillInTheBlankQuestion.blankSymbol.name}.")
    } else {
      StandardQuestion(text)
    }
  }
}

trait LongAnswerGenerator {
  /** Given a question and a short answer, this expands it into a long answer.
    *
    * For instance, suppose the question is "What type of fuel did the car use?" and the short
    * answer "gasoline", an appropriate long (sentential) answer might be: "The car used gasoline."
    *
    * @param question the question
    * @param shortAnswer the short (phrasal) answer
    * @return an answer to the question, in the form of a complete sentence
    */
  def generateLongAnswer(question: QuestionParse, shortAnswer: String): Option[String]
}

/** A StandardQuestion is the default. A non-specialized type of question.
  *
  * @param text the raw question text
  */
case class StandardQuestion(text: String) extends QuestionParse

class StandardQuestionProcessor(fitbGenerator: FillInTheBlankGenerator)
  extends LongAnswerGenerator {

  private val fitbQuestionProcessor = new FillInTheBlankQuestionProcessor

  def generateLongAnswer(question: QuestionParse, shortAnswer: String): Option[String] = {
    val trimmedShortAnswer = Util.trimPunctuation(shortAnswer)
    fitbGenerator.generateFITB(question) match {
      case Some(fitbQuestion) =>
        fitbQuestionProcessor.generateLongAnswer(fitbQuestion, shortAnswer)
      case None =>
        question match {
          case stdQuestion: StandardQuestion =>
            Some(s"${trimmedShortAnswer} is ${Util.lowercaseFirstCharacter(stdQuestion.text)}")
          case _ => None
        }
    }
  }
}

class StandardQuestionProcessor2(questionParser: TransitionParser) extends LongAnswerGenerator {

  private val rotator = new Rotator(Set(
    RotationRule("root", "dobj", "nsubj"),
    RotationRule("root", "dobj", "aux"),
    RotationRule("root", "aux", "nsubj"),
    RotationRule("root", "aux", "nsubjpass"),
    RotationRule("root", "auxpass", "nsubjpass"),
    RotationRule("root", "dobj", "head")
  ))

  def generateLongAnswer(question: QuestionParse, shortAnswer: String): Option[String] = {
    val trimmedShortAnswer = Util.trimPunctuation(shortAnswer)
    question match {
      case stdQuestion: StandardQuestion =>
        val longAnswer =
          Parser.parseUntokenizedSentence(questionParser, stdQuestion.text) flatMap { parse =>
            val rotatedParse = rotator.rotate(parse.asConstituencyParse)
            StandardQuestionProcessor.findQuestionNode(rotatedParse) map { questionNode =>
              val substituted = rotatedParse.substitute(
                questionNode,
                new SubstitutionTree(trimmedShortAnswer)
              )
              ConstituencyParse.getWords(substituted).mkString(" ")
            }
          }
        longAnswer match {
          case None =>
            Some(s"${trimmedShortAnswer} is ${Util.lowercaseFirstCharacter(stdQuestion.text)}")
          case x => x
        }
      case _ => None
    }
  }
}

object StandardQuestionProcessor {

  /** Searches through a constituency parse for the most likely node to correspond to the question.
    *
    * Right now this covers only a fraction of question types (specifically, "which", "what"
    * and "who"). The coverage will increase in Q1 2015.
    *
    * @param parse the constituency parse
    * @return the position of the node that corresponds to the question word
    */
  def findQuestionNode(parse: PositionTree): Option[Position] = {
    val filteredPositions = parse.positions filter { position =>
      val childWords: Seq[String] = parse.getChildren(position) flatMap { childPosition =>
        parse.getLabel(childPosition, ConstituencyParse.wordLabelName)
      }
      ((childWords map { x => x.toLowerCase }).toSet intersect Set("which", "what", "who")).nonEmpty
    }
    val mappedPositions = filteredPositions flatMap { position =>
      if (parse.getLabel(position, ConstituencyParse.constituencyLabelName) == Some("DET")) {
        position.parent
      } else {
        Some(position)
      }
    }
    mappedPositions.headOption
  }
}

/** A FillInTheBlankQuestion is an abstraction for a fill-in-the-blank style question.
  *
  * @param text the raw question text
  */
case class FillInTheBlankQuestion(text: String) extends QuestionParse {
  val tokens: Seq[String] = defaultTokenizer.tokenize(text) map { tok =>
    tok.string
  }

  lazy val tokenizedString: String = tokens.mkString(" ")
}

object FillInTheBlankQuestion {
  val blankSymbol = Symbol("BLANK_")
}

class FillInTheBlankQuestionProcessor
  extends LongAnswerGenerator {

  def generateLongAnswer(question: QuestionParse, shortAnswer: String): Option[String] = {
    val trimmedShortAnswer = Util.trimPunctuation(shortAnswer)
    question match {
      case fbQuestion: FillInTheBlankQuestion =>
        val mappedTokens = fbQuestion.tokens map {
          case FillInTheBlankQuestion.blankSymbol.name => trimmedShortAnswer
          case tok => tok
        }
        Some(mappedTokens.mkString(" "))
      case _ => None
    }
  }
}

