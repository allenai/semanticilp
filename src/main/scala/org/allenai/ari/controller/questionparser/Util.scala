package org.allenai.ari.controller.questionparser

object Util {

  /** Converts the first character of a string to lowercase.
    *
    * @param str the string to convert
    * @return the same string, with the first character converted to lowercase
    */
  def lowercaseFirstCharacter(str: String): String = {
    str.headOption map { firstChar =>
      firstChar.toString.toLowerCase + str.tail
    } getOrElse ("")
  }

  /** Converts the first character of a string to uppercase.
    *
    * @param str the string to convert
    * @return the same string, with the first character converted to uppercase
    */
  def uppercaseFirstCharacter(str: String): String = {
    str.headOption map { firstChar =>
      firstChar.toString.toUpperCase + str.tail
    } getOrElse ("")
  }

  /** Trims punctuation and trailing whitespace from the end of a string.
    *
    * Currently this only trims a single period or question mark.
    *
    * @param str the string to trim
    * @return the trimmed string
    */
  def trimPunctuation(str: String): String = {
    str.trim.lastOption match {
      case Some(lastChar) =>
        if (lastChar == '.' || lastChar == '?') {
          str.dropRight(1).trim
        } else {
          str.trim
        }
      case None => ""
    }
  }

  /** Processes a line in tab-separated value (TSV) format.
    *
    * TODO: doubtful that this correctly handles fields with quotes in them
    *
    * @param line the TSV-formatted line
    * @return an indexed sequence of the tab-separated string fields
    */
  def readTabSeparatedValueLine(line: String): IndexedSeq[String] = {
    val lineFields: Array[String] = line.split("\t")
    lineFields map { str =>
      if (str.size > 1 && str.head == '"' && str.last == '"') {
        str.slice(1, str.size - 1)
      } else {
        str
      }
    }
  }
}
