package org.allenai.ari.solvers.textilp

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import org.allenai.ari.solvers.textilp.utils.{AnnotationUtils, Constants, SQuADReader}
import org.rogach.scallop._

import scala.collection.JavaConverters._

object ExperimentsApp {

  class ArgumentParser(args: Array[String]) extends ScallopConf(args) {
    val experimentType: ScallopOption[Int] = opt[Int]("type", descr = "Experiment type", required = true)
    verify()
  }

/*  def evaluateSquadWithAristo(inputData: Seq[TopicGroup]) = {

  }
*/
  def generateCandiateAnswers(): Unit = {
    //val trainReader = new SQuADReader(Constants.squadTrainingDataFile, Some(AnnotationUtils.pipelineService))
    val devReader = new SQuADReader(Constants.squadDevDataFile, Some(AnnotationUtils.pipelineService))
    devReader.instances.slice(0, 1).foreach{ ins =>
      ins.paragraphs.foreach{ p =>
        println("p = " + p)
        p.contextTAOpt match {
          case None => throw new Exception("The instance does not contain annotation . . . ")
          case Some(annotation) =>
            val surfaceString = annotation.getView(ViewNames.NER_CONLL).getConstituents.asScala.map(_.getSurfaceForm).mkString("|")
            println(surfaceString)
            val quant = annotation.getView(ViewNames.QUANTITIES).getConstituents.asScala.map(_.getSurfaceForm).mkString("|")
            println(quant)
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val parser = new ArgumentParser(args)
    parser.experimentType() match {
      case 1 =>
        generateCandiateAnswers()
      case 2 =>
    }
  }
}
