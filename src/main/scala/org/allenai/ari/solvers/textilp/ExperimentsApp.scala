package org.allenai.ari.solvers.textilp

import org.allenai.ari.solvers.textilp.utils.SQuADReader

object ExperimentsApp {
  def main(args: Array[String]): Unit = {
    val trainData = "/Users/daniel/Desktop/squad-train-v1.1.json"
    val testData = "/Users/daniel/Desktop/squad-dev-v1.1.json"
    val trainReader = new SQuADReader(trainData)
    val devReader = new SQuADReader(testData)
    println(devReader.instances.head)
  }
}
