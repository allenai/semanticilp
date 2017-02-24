package org.allenai.ari.controller.questionparser

import org.allenai.nlpstack.parse.poly.core.{ ConstituencyParse, SubstitutionTree }
import org.allenai.nlpstack.parse.poly.polyparser.{ Parser, TransitionParser }

/** The RotatingFITBGenerator produces a constituency parse of an interrogative question,
  * rotates the top-level nodes as appropriate (according to a set of deterministic rules),
  * and then substitutes the question constituent with a BLANK_ symbol.
  *
  * For instance, the rule RotationRule("root", "aux", "nsubj") says that for any node
  * labeled "root", we should swap any children labeled "aux" and "nsubj" if the node labeled
  * "aux" originally appears to the left of the node labeled "nsubj". This means that the
  * question "Which bike did he ride?" would become (after rotation) "Which bike he did ride?"
  * (since "did" is an AUX and "he" is a NSUBJ in the original interrogative question).
  *
  * @param questionParser the parser to use for parsing the interrogative questions
  */
case class RotatingFITBGenerator(questionParser: TransitionParser) extends FillInTheBlankGenerator {

  @transient private val rotator = new Rotator(Set(
    RotationRule("root", "dobj", "nsubj"),
    RotationRule("root", "dobj", "aux"),
    RotationRule("root", "aux", "nsubj"),
    RotationRule("root", "aux", "nsubjpass"),
    RotationRule("root", "auxpass", "nsubjpass"),
    RotationRule("root", "dobj", "head")
  ))

  override def generateFITB(question: QuestionParse): Option[FillInTheBlankQuestion] = {
    question match {
      case stdQuestion: StandardQuestion =>
        val questionParse = Parser.parseUntokenizedSentence(questionParser, stdQuestion.text)
        val text: Option[String] = questionParse flatMap { parse =>
          val rotatedParse = rotator.rotate(parse.asConstituencyParse)
          StandardQuestionProcessor.findQuestionNode(rotatedParse) map { questionNode =>
            val substituted = rotatedParse.substitute(
              questionNode,
              new SubstitutionTree(FillInTheBlankQuestion.blankSymbol.name)
            )
            ConstituencyParse.getWords(substituted).mkString(" ")
          }
        }
        // post-hoc modifications: final question mark becomes a period and
        // the first letter is capitalized
        val finalLetterPeriod: Option[String] = text map { x: String =>
          if (x.lastOption == Some('?')) {
            x.dropRight(1) + "."
          } else {
            x
          }
        }
        finalLetterPeriod map { x =>
          Util.uppercaseFirstCharacter(x)
        } map { x =>
          FillInTheBlankQuestion(x)
        }
      case fitbQuestion: FillInTheBlankQuestion => Some(fitbQuestion)
      case _ => None
    }
  }
}
