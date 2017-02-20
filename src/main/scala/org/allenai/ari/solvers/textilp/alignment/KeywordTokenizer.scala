package org.allenai.ari.solvers.textilp.alignment

import com.google.inject.name.Named
import com.google.inject.{ Inject, Singleton }
import org.allenai.common.Resource
import org.allenai.datastore.Datastore
import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.tokenize.FactorieTokenizer

import scala.io.Source

/** An arilog-specific tokenizer, used for extracting keyword tokens from ISA text. This handles
  * stemming, stopword filtering, and substitution mapping; plus it has special logic to ignore ISA
  * variables (strings starting with '?').
  * @param stopwords a set of strings to filter out of keyword results
  * @param substitutions a mapping of word to substitution text, to replace in keyword results
  * during stemming
  */
@Singleton class KeywordTokenizer @Inject() (
  @Named("tokenizerStopwords") stopwords: Set[String],
  @Named("tokenizerSubstitutions") substitutions: Map[String, String]
) {
  /** Regular expression matching any character that's not allowed in a keyword. We allow any letter
    * or digit, plus spaces and question marks.
    */
  val disallowedInKeyword = """[^\p{L}\p{Nd}\? ]""".r

  /** Internal tokenizer instance. FactorieTokenizer is thread-safe. */
  private val tokenizer = new FactorieTokenizer

  /** @return true if the given token is alphanumeric-ish and not a stopword */
  def isKeyword(token: String): Boolean = {
    disallowedInKeyword.findFirstMatchIn(token).isEmpty && !stopwords.contains(token)
  }

  /** Returns the token strings for the given string. This special-cases ISA variable strings
    * (strings starting with "?") by returning them unmodified.
    */
  def rawTokenize(string: String): Seq[String] = {
    if (string.startsWith("?")) {
      Seq(string)
    } else {
      tokenizer.tokenize(string) map { _.string }
    }
  }

  /** Returns the lower-cased stemmed version of the given word. This will look up the lower-cased
    * word in the substitutions map before stemming it with the Morpha stemmer.
    */
  def stem(word: String): String = {
    // TODO(jkinkead): This doesn't do the right thing with proper nouns that look like a verb or
    // plural (like Lansing or Kansas).
    val lowerCase = word.toLowerCase
    // Use a substitution if we have it.
    val substitution = substitutions.getOrElse(lowerCase, lowerCase)
    MorphaStemmer.stem(substitution)
  }

  /** Tokenizes the given string, stems the tokens using the substitution map and Morpha stemmer,
    * and filters for non-keyword tokens and stopwords. Returns the result.
    */
  def stemmedKeywordTokenize(string: String): Seq[String] = {
    for {
      token <- rawTokenize(string)
      stemmedWord = stem(token) if isKeyword(stemmedWord)
    } yield stemmedWord
  }

  /** Tokenizes the given string, stems the tokens using the substitution map and Morpha stemmer.
    */
  def stemmedTokenize(string: String): Seq[String] = {
    for {
      token <- tokenizer.tokenize(string).map(_.string)
      stemmedWord = stem(token)
    } yield stemmedWord
  }

  /** Tokenizes the given string, and filters for non-keyword tokens and stopwords. Returns the
    * result.
    */
  def keywordTokenize(string: String): Seq[String] = {
    for (token <- rawTokenize(string) if isKeyword(token)) yield token
  }
}
object KeywordTokenizer {
  /** The default tokenizer instance. */
  lazy val Default = new KeywordTokenizer(defaultStopwords, defaultSubstitutions)

  /** The default stopwords set, sourced from Peter Clark. */
  lazy val defaultStopwords: Set[String] = {
    Resource.using(Source.fromFile(Datastore.filePath("org.allenai.nlp.resources", "stopwords.txt", 1).toFile)) { source =>
      source.getLines().toSet
    }
  }

  /** The default substitutions map loaded from the class resources "comparatives.txt" and
    * "morphgroups.txt"
    */
  lazy val defaultSubstitutions: Map[String, String] = {
    loadSubstitutionResource("comparatives.txt") ++ loadSubstitutionResource("morphgroups.txt")
  }

  /** Substitution files are three tokens per line, with the base form in the first column and the
    * alternate forms in the next two columns. This returns a mapping from the alternates to the
    * base for both alternates.
    */
  def loadSubstitutionResource(name: String): Map[String, String] = {
    val substitutionStream = getClass.getResourceAsStream(name)
    Resource.using(Source.fromInputStream(substitutionStream)) { input =>
      (
        for {
          line <- input.getLines()
          splits = line.split(" ") if (splits.size == 3)
        } yield Seq(splits(1) -> splits(0), splits(2) -> splits(0))
      ).flatten.toMap
    }
  }
}
