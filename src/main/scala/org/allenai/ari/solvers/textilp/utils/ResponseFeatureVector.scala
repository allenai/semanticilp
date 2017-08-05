package org.allenai.ari.solvers.textilp.utils

import spray.json._

/** A ResponseFeatureVector represents a set of features associated with answers across all solvers.
  * @param questionHash A short hash of original question text to identify groups of answers
  * belonging together
  * @param answerKey The key associated with an answer, such as "(A)"
  * @param isSolution Indicates whether the answer is the correct one, only filled in by the
  * evaluation framework
  * @param numericFeatures The set of numeric features
  */
case class ResponseFeatureVector(
  questionHash: String,
  answerKey: String,
  isSolution: Option[Boolean] = None,
  numericFeatures: Map[String, Double]
) {

  /** Retrieves a given feature.
    * @return 0 if feature is not available, or 1 for special "intercept" feature
    */
  def getNumericFeature(feature: String): Double =
    if (feature.matches("[Ii]ntercept")) 1.0 else numericFeatures.getOrElse(feature, 0d)

  def updateNumericFeature(feature: String, value: Double): ResponseFeatureVector = {
    this.copy(numericFeatures = this.numericFeatures.updated(feature, value))
  }
}

object ResponseFeatureVector {
  /** Used for indicating feature vectors in the answer analysis */
  val FeatureVectorsProperty = "featureVectors"

  /** Generates CSV output including a header row */
  def toCsv(featureVectors: Seq[ResponseFeatureVector]): String = {
    val allNumericFeatures =
      (featureVectors flatMap (fv => fv.numericFeatures.keys)).distinct.sorted
    val header = (
      Seq("questionHash", "answerKey") ++
      allNumericFeatures :+
      "isSolution"
    ).mkString(",")
    val lines = for {
      fv <- featureVectors
      isSolution <- fv.isSolution
    } yield {
      (
        Seq(fv.questionHash, fv.answerKey) ++
        (allNumericFeatures map fv.getNumericFeature) :+
        { if (isSolution) 1 else 0 }
      ).mkString(",")
    }
    (header +: lines).mkString("\n")
  }

  /** Construct feature vectors from CSV input, where first row is list of features followed by the
    * data
    */
  def fromCsv(lines: Seq[String]): Seq[ResponseFeatureVector] = {
    require(lines.nonEmpty)
    val allFeatures = lines.head.split(",") map (_.trim)
    val specialFeatures = Seq("questionHash", "answerKey", "isSolution")
    for {
      line <- lines.tail
      fMap = allFeatures.zip(line.split(",") map (_.trim)).toMap
      if fMap.size > 1
    } yield {
      ResponseFeatureVector(
        questionHash = fMap.getOrElse("questionHash", ""),
        answerKey = fMap.getOrElse("answerKey", ""),
        isSolution = fMap.getOrElse("isSolution", "") match {
          case "1" | "1." | "1.0" => Some(true)
          case "0" | "0." | "0.0" => Some(false)
          case _ => None
        },
        numericFeatures = fMap.filterKeys(k => !specialFeatures.contains(k)) map {
          case (key, value) => key -> value.toDouble
        }
      )
    }
  }

  import DefaultJsonProtocol._
  implicit val ResponseFeatureVectorFormat = jsonFormat4(ResponseFeatureVector.apply)
}

case class LinearConfidenceModel(featureWeights: Map[String, Double])
