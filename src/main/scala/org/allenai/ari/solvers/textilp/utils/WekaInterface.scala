package org.allenai.ari.solvers.textilp.utils

import weka.classifiers.Classifier
import weka.classifiers.bayes.NaiveBayes
import weka.classifiers.functions.{ LibSVM, Logistic, SMO }
import weka.classifiers.`lazy`.IBk
import weka.classifiers.meta.RandomCommittee
import weka.classifiers.rules.{ DecisionTable, JRip, OneR, ZeroR }
import weka.classifiers.trees.{ J48, REPTree, RandomForest }
import weka.core.{ Attribute, DenseInstance, Instances }

import java.util.ArrayList
import scala.collection.JavaConverters._

/* This object collects code interacting with the Weka library */
object WekaInterface {

  /* Construct a Weka dataset from a set of feature vectors and a desired set of features */
  def makeWekaDataset(
    featureVectors: Seq[ResponseFeatureVector],
    featureSet: Seq[String]
  ): Instances = {
    val isSolutionAttr = new Attribute("isSolution", Seq("false", "true").toList.asJava)
    val numericAttrs = featureSet map (f => new Attribute(f))
    val attrs = new ArrayList[Attribute]((numericAttrs :+ isSolutionAttr).toList.asJava)
    val dataset = new Instances("Dataset", attrs, featureVectors.size)
    val dataSize = featureSet.size + 1
    dataset.setClassIndex(dataSize - 1)
    for {
      fv <- featureVectors
      entry = new DenseInstance(dataSize)
    } {
      numericAttrs.zip(featureSet) map { case (a, f) => entry.setValue(a, fv.getNumericFeature(f)) }
      entry.setValue(isSolutionAttr, { if (fv.isSolution == Some(true)) "true" else "false" })
      dataset.add(entry)
    }
    dataset
  }

  class WekaConfidencePredictor(val model: Classifier, val baseDataset: Instances) {

    def this(model: Classifier, featureSet: Seq[String]) =
      this(model, makeWekaDataset(Seq(), featureSet))
    val name = "WekaConfidencePredictor"

    val dataSize = baseDataset.numAttributes
    val numericAttrsZip = for (i <- 0 to dataSize - 2) yield {
      (baseDataset.attribute(i), baseDataset.attribute(i).name)
    }

    def predictConfidence(featureVector: ResponseFeatureVector): Double = {
      val entry = new DenseInstance(dataSize)
      numericAttrsZip map { case (a, f) => entry.setValue(a, featureVector.getNumericFeature(f)) }
      entry.setDataset(baseDataset)
      model.distributionForInstance(entry)(1)
    }
  }

  val activeWekaClassifiers = Seq("DecisionTable", "IBk", "J48", "JRip",
    "Logistic", "NaiveBayes", "RandomCommittee", "RandomForest", "REPTree",
    "SMO", "ZeroR", "OneR")

  def newWekaClassifier(
    classifierName: String,
    featureSet: Seq[String]
  ): WekaConfidencePredictor = {
    val model = classifierName match {
      //case "ConjunctiveRule" => new ConjunctiveRule()
      case "DecisionTable" => new DecisionTable()
      case "IBk" => new IBk()
      case "J48" => new J48()
      case "JRip" => new JRip()
      case "LibSVM" => new LibSVM()
      case "Logistic" => new Logistic()
      //case "LogisticRidge0" => {val cl = new Logistic(); cl.setRidge(0.0); cl}
      case "NaiveBayes" => new NaiveBayes()
      case "RandomCommittee" => new RandomCommittee()
      case "RandomForest" => new RandomForest()
      //case "RBFNetwork" => new RBFNetwork()
      case "REPTree" => new REPTree()
      //case "RotationForest" => new RotationForest()
      case "SMO" => new SMO()
      case "ZeroR" => new ZeroR()
      case "OneR" => new OneR()
      case _ => throw new IllegalArgumentException(s"Unknown Weka classifier: $classifierName")
    }
    new WekaConfidencePredictor(model, featureSet)
  }

}