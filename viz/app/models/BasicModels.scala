package models

case class QuestionChoice(str: String)

case class TextSnippet(str: String)

case class Question(str: String, questionChoice: Seq[QuestionChoice], snippet: TextSnippet)

case class PreFilledQuestions(questions: Seq[Question])

sealed trait SolverType {}
case object TextILP extends SolverType
case object Salience extends SolverType
case object Lucene extends SolverType

sealed trait CandidateGeneration {}
case object CandidatesGiven extends CandidateGeneration
case object CandidatesAutomatic extends CandidateGeneration

sealed trait ExternalKnowledge {}
case object KnowledgeGiven extends ExternalKnowledge
case object LuceneKnowledge extends ExternalKnowledge

case class FormContent(
  preFilledQuestions: PreFilledQuestions,
  preFilledQuestionIndOpt: Option[Int],
  questionOpt: Option[Question],
  candidateGeneration: CandidateGeneration,
  externalKnowledge: ExternalKnowledge
)

object StaticContent {

}
