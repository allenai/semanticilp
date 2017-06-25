package org.allenai.ari.solvers.textilp

import edu.illinois.cs.cogcomp.core.datastructures.textannotation.TextAnnotation
import org.apache.commons.math3.util.Precision

import scala.collection.mutable.ArrayBuffer

case class TopicGroup(title: String, paragraphs: Seq[Paragraph])
case class QPPair(question: Question, paragraph: Paragraph, beginTokenIdx: Int, endTokenIdx: Int, scoreOpt: Option[Double] = None, sentenceIdOpt: Option[Int] = None)
case class Paragraph(context: String, questions: Seq[Question], contextTAOpt: Option[TextAnnotation])
case class Question(questionText: String, questionId: String, answers: Seq[Answer], qTAOpt: Option[TextAnnotation], correctIdxOpt: Option[Int] = None)
case class Answer(answerText: String, answerStart: Int, aTAOpt: Option[TextAnnotation] = None)

/** The alignment of a basic textual alignment unit (a term) in the ILP solution.
  * //  * @param term A basic alignment unit (a word, a chunk, a string associated with a cell, etc)
  * //  * @param alignmentIds A sequence of alignment IDs that connect this term with other terms
  * //
  */
//case class TermAlignment(term: String, alignmentIds: ArrayBuffer[Int] = ArrayBuffer.empty)
//
//case class AlignmentResults(
//   questionAlignments: List[TermAlignment] = List.empty,
//   choiceAlignments: List[TermAlignment] = List.empty,
//   paragraphAlignments: List[TermAlignment] = List.empty
//)

case class Entity(entityName: String, surface: String, boundaries: Seq[(Int, Int)])
case class Relation(relationName: String, entity1: String, entity2: String, weight: Double)
case class Stats(numberOfBinaryVars: Double = 0.0, numberOfContinuousVars: Double = 0.0,
  numberOfIntegerVars: Double = 0.0, numberOfConstraints: Double = 0.0, modelCreationInSec: Double = 0.0,
  solveTimeInSec: Double = 0.0, ilpIterations: Double = 0.0) {
  def asVector: Seq[Double] = {
    Seq(numberOfBinaryVars, numberOfContinuousVars, numberOfIntegerVars, numberOfConstraints,
      modelCreationInSec, solveTimeInSec, ilpIterations)
  }
  def sumWith(in: Seq[Double]): Seq[Double] = asVector.zip(in).map { case (x: Double, y: Double) => x + y }
  def sumWith(in: Stats): Stats = Stats(
    in.numberOfBinaryVars + numberOfBinaryVars,
    in.numberOfContinuousVars + numberOfContinuousVars,
    in.numberOfIntegerVars + numberOfIntegerVars,
    in.numberOfConstraints + numberOfConstraints,
    in.modelCreationInSec + modelCreationInSec,
    in.solveTimeInSec + solveTimeInSec,
    in.ilpIterations + ilpIterations
  )
  def divideBy(denominator: Int) = Stats(numberOfBinaryVars / denominator, numberOfContinuousVars / denominator,
    numberOfIntegerVars / denominator, numberOfConstraints / denominator, modelCreationInSec / denominator,
    solveTimeInSec / denominator, ilpIterations / denominator)

  override def toString: String = {
    s"numberOfBinaryVars: $numberOfBinaryVars \nnumberOfContinuousVars: $numberOfContinuousVars \n" +
      s"numberOfIntegerVars: $numberOfIntegerVars \nnumberOfConstraints: $numberOfConstraints \n" +
      s"modelCreationInSec: $modelCreationInSec \nsolveTimeInSec: $solveTimeInSec \nilpIterations: $ilpIterations"
  }
}
case class EntityRelationResult(
  fullString: String = "",
  entities: Seq[Entity] = Seq.empty,
  relations: Seq[Relation] = Seq.empty,
  explanation: String = "",
  statistics: Stats = Stats(),
  confidence: Double = -100.0, log: String = ""
)

object ResultJson {
  //  val staticAlignmentResult = AlignmentResults(
  //    List(
  //      TermAlignment("In"), TermAlignment("New York State", ArrayBuffer(0)), TermAlignment("the longest period"),
  //      TermAlignment("of"), TermAlignment("daylight"), TermAlignment("occurs"), TermAlignment("during"),
  //      TermAlignment("which"), TermAlignment("month"), TermAlignment("?")
  //    ),
  //    List(TermAlignment("June", ArrayBuffer(3)), TermAlignment("March"), TermAlignment("December"), TermAlignment("September")),
  //    List(
  //      TermAlignment("New York", ArrayBuffer(0)), TermAlignment("is"), TermAlignment("located", ArrayBuffer(2)), TermAlignment("in"),
  //      TermAlignment("the United States", ArrayBuffer(1)), TermAlignment("of"), TermAlignment("America"), TermAlignment("."),
  //      TermAlignment("The"), TermAlignment("USA", ArrayBuffer(1)), TermAlignment("is"), TermAlignment("located", ArrayBuffer(2)), TermAlignment("in"),
  //      TermAlignment("the northern hemisphere", ArrayBuffer(3)), TermAlignment(".")
  //    )
  //  )

  val staticEntityRelationResults = EntityRelationResult(
    "Question: In New York State, the longest period of daylight occurs during which month? |Options: (A) June  (B) March  (C) December  (D) September " +
      "|Paragraph: New York is located in United States. USA is located in northern hemisphere. The summer solstice happens during summer, in northern hemisphere.",
    List(Entity("T1", "New York State", Seq((1, 5))), Entity("T2", "United States", Seq((10, 15))),
      Entity("T3", "USA", Seq((20, 25))), Entity("T4", "northern hemisphere", Seq((30, 35)))),
    List(Relation("R1", "T1", "T2", 0.0), Relation("R2", "T2", "T3", 0.0), Relation("R3", "T3", "T4", 0.0))
  )

  val emptyEntityRelation = EntityRelationResult("", List.empty, List.empty)

  import play.api.libs.json._

  // writing answer-paragraphs
  implicit val answerWrites = new Writes[Answer] {
    def writes(answer: Answer) = Json.arr(answer.answerText)
  }

  implicit val questionWrites = new Writes[Question] {
    def writes(q: Question) = Json.obj(
      "question" -> q.questionText,
      "answers" -> q.answers,
      "correctAns" -> q.correctIdxOpt.get
    )
  }

  implicit val paragraphWrite = new Writes[Paragraph] {
    def writes(p: Paragraph) = Json.obj(
      "paragraphText" -> p.context,
      "paragraphQuestions" -> p.questions
    )
  }

  implicit val listOfparagraphWrite = new Writes[List[Paragraph]] {
    def writes(p: List[Paragraph]) = Json.arr(p)
  }

  // writing entity relations
  implicit val entityWrites = new Writes[Entity] {
    def writes(entity: Entity) = Json.arr(entity.entityName, entity.surface, entity.boundaries.map(pair => Json.arr(pair._1, pair._2)))
  }

  implicit val relationWrites = new Writes[Relation] {
    def writes(relation: Relation) = Json.arr(relation.relationName, s" ${Precision.round(relation.weight, 2)} ", Json.arr(Json.arr("  ", relation.entity1), Json.arr("  ", relation.entity2)))
  }

  implicit val entityRelationWrites = new Writes[EntityRelationResult] {
    def writes(er: EntityRelationResult) = Json.obj(
      "overalString" -> er.fullString,
      "entities" -> er.entities,
      "relations" -> er.relations,
      "explanation" -> er.explanation,
      "log" -> er.log
    )
  }

  //  implicit val termWrites = new Writes[TermAlignment] {
  //    def writes(term: TermAlignment) = Json.obj(
  //      "term" -> term.term,
  //      "alignmentIds" -> term.alignmentIds
  //    )
  //  }
  //
  //  implicit val results = new Writes[AlignmentResults] {
  //    def writes(term: AlignmentResults) = Json.obj(
  //      "questionAlignments" -> term.questionAlignments,
  //      "choiceAlignments" -> term.choiceAlignments,
  //      "paragraphAlignments" -> term.paragraphAlignments
  //    )
  //  }
}

/** Writing the results in the format that squad (hence BiDaF expcts)
  */
object SquadJsonPattern {

  import play.api.libs.json._

  implicit val listOfparagraphWrite = new Writes[List[Paragraph]] {
    def writes(p: List[Paragraph]) = {
      Json.obj(
        "data" -> p.zipWithIndex.map {
          case (pp, pIdx) =>
            val firstWord = pp.context.split(" ").head
            Json.obj(
              "title" -> "bestTitleEva",
              "paragraphs" ->
                Json.arr(
                  Json.obj(
                    "context" -> pp.context,
                    "qas" -> pp.questions.zipWithIndex.map {
                      case (q, qIdx) =>
                        Json.obj(
                          "question" -> q.questionText,
                          "answers" ->
                            Json.arr(
                              Json.obj("text" -> firstWord, "answer_start" -> 0)
                            ),
                          "id" -> s"$pIdx-$qIdx"
                        )
                    }
                  )
                )
            )
        }
      )
    }
  }
}

