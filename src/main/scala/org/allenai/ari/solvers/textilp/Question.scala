package org.allenai.ari.solvers.textilp

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation

case class TopicGroup(title: String, paragraphs: Seq[Paragraph])
case class Paragraph(context: String, questions: Seq[Question], contextTAOpt: Option[TextAnnotation])
case class Question(questionText: String, questionId: String, answers: Seq[Answer], qTAOpt: Option[TextAnnotation])
case class Answer(answerText: String, answerStart: Int)

object ReaderHelpers {
  /*def fromJson(string): Seq[TopicGroup] = {

  }*/
}