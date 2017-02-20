package org.allenai.ari.solvers.textilp.solvers
import edu.illinois.cs.cogcomp.McTest.MCTestBaseline
import org.allenai.ari.solvers.textilp.EntityRelationResult
import org.allenai.ari.solvers.textilp.utils.SolverUtils

import scala.collection.JavaConverters._

/** Using it with the default settings used for MCTest
  * Might need a little tuning for each dataset
  */
class SlidingWindowSolver extends TextSolver {
  MCTestBaseline.readStopWords()
  val stopwords = MCTestBaseline.stopWords.asScala.toSet
  assert(stopwords.size > 20)
  override def solve(question: String, options: Seq[String], snippet: String): (Seq[Int], EntityRelationResult) = {
    val scores = MCTestBaseline.ScoreAnswers(snippet.split(" "), question, options.toArray, MCTestBaseline.stopWords)
    SolverUtils.sortedAnswerToSolverResponse(question, options, snippet, options.zip(scores))
  }
}
