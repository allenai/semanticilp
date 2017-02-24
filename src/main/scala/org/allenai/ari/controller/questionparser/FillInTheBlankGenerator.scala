package org.allenai.ari.controller.questionparser

import org.allenai.nlpstack.parse.PolytreeParser

trait FillInTheBlankGenerator {

  /** Converts an interrogative question into a fill-in-the-blank question.
    *
    * For instance, suppose the question is "What type of fuel did the car use?", an appropriate
    * fill-in-the-blank counterpart might be: "The car used BLANK_." with the auxiliary statement
    * "BLANK_ is a type of fuel."
    *
    * @param question the interrogative question
    * @return a fill-in-the-blank analog to the argument question
    */
  def generateFITB(question: QuestionParse): Option[FillInTheBlankQuestion]
}

object FillInTheBlankGenerator {
  lazy val mostRecent: FillInTheBlankGenerator = new RotatingFITBGenerator(
    new PolytreeParser().parser
  )
}
