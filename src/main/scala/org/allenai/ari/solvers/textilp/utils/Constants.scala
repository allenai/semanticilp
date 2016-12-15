package org.allenai.ari.solvers.textilp.utils

import java.io.File

object Constants {
  val squadTrainingDataFile = new File("/Users/daniel/Desktop/squad-train-v1.1.json")
  val squadDevDataFile = new File("/Users/daniel/Desktop/squad-dev-v1.1.json")
  val queryLink = "http://aristo-dev.dev.ai2:8080/ask?text="
  val useRedisCachingForAnnotation = false
}
