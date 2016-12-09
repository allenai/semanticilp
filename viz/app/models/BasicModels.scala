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
                        solverType: SolverType,
  preFilledQuestions: PreFilledQuestions,
  preFilledQuestionIndOpt: Option[Int],
  questionOpt: Option[Question],
  candidateGeneration: CandidateGeneration,
  externalKnowledge: ExternalKnowledge,
  solverLog: String
)

object StaticContent {
  val preFilledQuestions = PreFilledQuestions(
    Seq(
      Question(
        "What was Nikola Tesla's ethnicity?",
        Seq(
          QuestionChoice("Serbian"), QuestionChoice("African"), QuestionChoice("American"), QuestionChoice("Asian")
        ),
        TextSnippet("Nikola Tesla (Serbian Cyrillic: Никола Тесла; 10 July 1856 – 7 January 1943) was a Serbian American inventor, electrical engineer, mechanical engineer, physicist, and futurist best known for his contributions to the design of the modern alternating current (AC) electricity supply system.")
      ),
      Question(
        "A decomposer is an organism that",
        Seq(
          QuestionChoice("hunts and eats animals"), QuestionChoice("migrates for the winter"), QuestionChoice("breaks down dead plants and animals"), QuestionChoice("uses water and sunlight to make food")
        ),
        TextSnippet("explanation:Decomposers: organisms that obtain energy by eating dead plant or animal matter. " +
          "Windy, cloudy, rainy, and cold are words that help describe\tfocus: deposition. " +
          "explanation:DECOMPOSER An organism that breaks down cells of dead plants and animals into simpler substances." +
          "explanation:The plants use sunlight, carbon dioxide, water, and minerals to make food that sustains themselves and other organisms in the forest.")
      )
    )
  )

  val sampleFormContent = FormContent(
    Lucene,
    preFilledQuestions,
    None,
    Some(preFilledQuestions.questions.head),
    CandidatesAutomatic,
    LuceneKnowledge,
    ""
  )

  val initialFormContent = FormContent(
    TextILP,
    preFilledQuestions,
    None,
    Some(Question("", Seq.empty, TextSnippet(""))),
    CandidatesGiven,
    KnowledgeGiven,
    ""
  )

  def getContentWithPrefilled(index: Int): FormContent = {
    initialFormContent.copy(questionOpt = Some(preFilledQuestions.questions(index)))
  }
}
