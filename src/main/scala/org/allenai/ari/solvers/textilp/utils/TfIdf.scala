package org.allenai.ari.solvers.textilp.utils

class TfIdf(documentList: Seq[String]) {

  private val numDocsContainingWord = scala.collection.mutable.Map[String, Int]()

  def score(word: String, document: String) = tf(word, document) * idf(word)

  private[this] def tf(word: String, document: String) = {
    val documentTokens = document.toLowerCase.split(" +")
    val freq = (word: String, document: String) => {
      val documentWordFrequency = documentTokens.groupBy(e => e).map(e => e._1 -> e._2.length)
      documentWordFrequency.getOrElse(word.toLowerCase(), 0)
    }
    freq(word, document).toDouble / documentTokens.size
  }

  def getDocumentCount(word: String): Int = {
    documentList.foldLeft(0) {
      (acc, e) => if (e.toLowerCase.contains(word)) acc + 1 else acc
    }
  }

  private[this] def idf(word: String) = {
    val numDocsContaining = numDocsContainingWord.get(word) match {
      case None =>
        val count = getDocumentCount(word)
        numDocsContainingWord.put(word, count)
        count
      case Some(x) => x
    }

    Math.log(documentList.length) / numDocsContaining
  }

}
