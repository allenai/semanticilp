package org.allenai.ari.solvers.textilp.alignment

import java.io.File

import com.medallia.word2vec.Word2VecModel
import com.redis.RedisClient
import com.typesafe.config.ConfigFactory
import org.allenai.ari.solvers.textilp.utils.Constants
import org.allenai.common.Logging
import org.allenai.entailment.Entailer
import org.allenai.entailment.interface.{Entailment, Postag}
import org.allenai.nlpstack.core.{PostaggedToken, Token}

/** Various options for computing similarity */
sealed trait SimilarityType {
  def scoreTitleTitle(titleStr1: String, titleStr2: String,
    context1Opt: Option[String] = None, context2Opt: Option[String] = None): Double // should be symmetric
  def scoreCellCell(cellStr1: String, cellStr2: String,
    context1Opt: Option[String] = None, context2Opt: Option[String] = None): Double // should be symmetric
  def scoreCellQCons(cellStr: String, qConsStr: String,
    context1Opt: Option[String] = None, context2Opt: Option[String] = None): Double // directional: qCons to cell
  def scoreTitleQCons(titleStr: String, qConsStr: String,
    context1Opt: Option[String] = None, context2Opt: Option[String] = None): Double // directional: qCons to title
  def scoreCellQChoice(cellStr: String, qChoiceStr: String,
    context1Opt: Option[String] = None): Double // directional: cell to qChoice
  def scoreTitleQChoice(titleStr: String, qChoiceStr: String,
    context1Opt: Option[String] = None): Double // dir.: title to qChoice
  def scoreStrToWhTerms(str: String, whTerms: Seq[String]): Double // dir.: str to (max of) whTerms

  // turn a one-sided score into a symmetric one
  protected def getSymmetricScore(text1: String, text2: String, context1: Option[String], context2: Option[String],
    scoringFunction: (String, String, Option[String], Option[String]) => Double): Double = {
    (scoringFunction(text1, text2, context1, context2) + scoringFunction(text2, text1, context1, context2)) / 2d
  }

  protected def getSymmetricScore(text1: String, text2: String, scoringFunction: (String, String) => Double): Double = {
    (scoringFunction(text1, text2) + scoringFunction(text2, text1)) / 2d
  }

  // take the max of scores across various hypothesis strings
  protected def getMaxScore(text1: String, text2Seq: Seq[String],
    scoringFunction: (String, String) => Double): Double = {
    text2Seq.map(scoringFunction(text1, _)).max
  }

  protected def getMaxScore(text1: String, text2Seq: Seq[String], context1Opt: Option[String],
    context2OptSeq: Seq[Option[String]],
    scoringFunction: (String, String, Option[String], Option[String]) => Double): Double = {
    text2Seq.zip(context2OptSeq).map {
      case (text2, context2Opt) =>
        scoringFunction(text1, text2, context1Opt, context2Opt)
    }.max
  }
}

/** A function to compute alignment scores between paris of cells, title, question constituent, etc.
  * @param alignmentType Must be one of Entailment, WordOverlap, or Word2Vec
  * @param entailmentScoreOffset The value to subtract from raw entailment score to get the score
  * @param tokenizer A keyword tokenizer
  * @param useContextInRedisCache whether to use context in keys, if using redis for caching
  */
class AlignmentFunction(
    alignmentType: String,
    entailmentScoreOffset: Double,
    tokenizer: KeywordTokenizer,
    useRedisCache: Boolean,
    useContextInRedisCache: Boolean
) extends Logging {
  if (useContextInRedisCache) require(useRedisCache, "if you want to use context caching, you have to " +
    "enable Redis caching . . . ")
  private val similarityFunction: SimilarityType = alignmentType match {
    case "Entailment" => {
      logger.info("Using entailment for alignment score computation")
      if (useRedisCache) logger.info("  Using Redis cache for entailment scores")
      new EntailmentSimilarity(entailmentScoreOffset, tokenizer, useRedisCache, useContextInRedisCache)
    }
    case "Word2Vec" => {
      logger.info("Using word2vec for alignment score computation")
      new Word2VecSimilarity
    }
    case "WordOverlap" => {
      logger.info("Using word overlap for alignment score computation")
      new WordOverlapSimilarity(tokenizer)
    }
    case _: String => {
      throw new IllegalArgumentException(s"Alignment type $alignmentType not recognized")
    }
  }

  /** Alignment score between two titles of tables */
  def scoreTitleTitle(titleStr1: String, titleStr2: String, context1: Option[String] = None,
    context2: Option[String] = None): Double = {
    similarityFunction.scoreTitleTitle(titleStr1, titleStr2, context1, context2)
  }

  /** Alignment score between cells of two tables */
  def scoreCellCell(cellStr1: String, cellStr2: String, context1: Option[String] = None,
    context2: Option[String] = None): Double = {
    similarityFunction.scoreCellCell(cellStr1, cellStr2, context1, context2)
  }

  /** Alignment score between a title of a table, and a question constituent */
  def scoreTitleQCons(titleStr: String, qConsStr: String, context1: Option[String] = None,
    context2: Option[String] = None): Double = {
    similarityFunction.scoreTitleQCons(titleStr, qConsStr, context1, context2)
  }

  /** Alignment score between a cell of a table, and a question constituent */
  def scoreCellQCons(cellStr: String, qConsStr: String, context1: Option[String] = None,
    context2: Option[String] = None): Double = {
    similarityFunction.scoreCellQCons(cellStr, qConsStr, context1, context2)
  }

  /** Alignment score between a title of a table, and a question option */
  def scoreTitleQChoice(titleStr: String, qChoiceStr: String, context1: Option[String] = None): Double = {
    similarityFunction.scoreTitleQChoice(titleStr, qChoiceStr, context1)
  }

  /** Alignment score between a cell of a table, and a question option */
  def scoreCellQChoice(cellStr: String, qChoiceStr: String, context1: Option[String] = None): Double = {
    similarityFunction.scoreCellQChoice(cellStr, qChoiceStr, context1)
  }

  /** Alignment score between a string and a which term */
  private val spaceSep = " ".r
  private val semicolonSep = ";".r
  def scoreStrToWhTerms(str: String, whTerms: Seq[String]): Double = {
    if (whTerms.isEmpty) {
      0d
    } else {
      // very strict: returns 0 if str has more than 2 words after splitting by semi-colon, dropping
      // those enclosed in '[...]', and taking keywords
      // TODO: BEFORE MERGING WITH MASTER, CHANGE THIS CODE+COMPUTATION DUPLICATION with
      //   splitStemKeywordTokenizeFilter() called later
      val computableStr = (for {
        substr <- semicolonSep.split(str).toSeq
        trimmedStr = substr.trim
        if !trimmedStr.startsWith("[") || !trimmedStr.endsWith("]") // ignore strings like "[...]"
        if tokenizer.keywordTokenize(trimmedStr).length <= 2
      } yield trimmedStr).mkString("; ")
      if (whTerms.isEmpty || computableStr.isEmpty) {
        0d
      } else {
        similarityFunction.scoreStrToWhTerms(computableStr, whTerms)
      }
    }
  }
}

// how much does text1 entail text2? (directional); an entailment score below the offset value is
// considered negative correlation.
private class EntailmentSimilarity(
    entailmentScoreOffset: Double,
    tokenizer: KeywordTokenizer,
    useRedisCache: Boolean,
    useContextInRedisCaching: Boolean
) extends SimilarityType with Logging {

  private val redisOpt = if (useRedisCache) Some(new RedisClient(Constants.redisServer, Constants.redisPort)) else None

  def scoreTitleTitle(titleStr1: String, titleStr2: String, context1Opt: Option[String], context2Opt: Option[String]): Double = {
    val entailmentKey = if (useContextInRedisCaching) "titleTitleEnt||" else ""
    getSymmetricScore(titleStr1, titleStr2, context1Opt, context2Opt, getEntailmentScore(entailmentKey))
  }
  def scoreCellCell(cellStr1: String, cellStr2: String, context1Opt: Option[String], context2Opt: Option[String]): Double = {
    val entailmentKey = if (useContextInRedisCaching) "cellCellEnt||" else ""
    getSymmetricScore(cellStr1, cellStr2, context1Opt, context2Opt, getEntailmentScore(entailmentKey))
  }
  def scoreCellQCons(cellStr: String, qConsStr: String, context1Opt: Option[String], context2Opt: Option[String]): Double = {
    val entailmentKey = if (useContextInRedisCaching) "cellQConsEnt||" else ""
    getEntailmentScore(entailmentKey)(qConsStr, cellStr, context1Opt, context2Opt)
  }
  def scoreTitleQCons(titleStr: String, qConsStr: String, context1Opt: Option[String], context2Opt: Option[String]): Double = {
    val entailmentKey = if (useContextInRedisCaching) "titleQConsEnt||" else ""
    getEntailmentScore(entailmentKey)(qConsStr, titleStr, context1Opt, context2Opt)
  }
  def scoreCellQChoice(cellStr: String, qChoiceStr: String, context1Opt: Option[String]): Double = {
    val entailmentKey = if (useContextInRedisCaching) "cellQChoiceEnt||" else ""
    getEntailmentScore(entailmentKey)(cellStr, qChoiceStr, context1Opt, None)
  }
  def scoreTitleQChoice(titleStr: String, qChoiceStr: String, context1Opt: Option[String]): Double = {
    val entailmentKey = if (useContextInRedisCaching) "titleQChoiceEnt||" else ""
    getEntailmentScore(entailmentKey)(titleStr, qChoiceStr, context1Opt, None)
  }
  def scoreStrToWhTerms(str: String, whTerms: Seq[String]): Double = {
    val entailmentKey = if (useContextInRedisCaching) "strToWhTermsEnt||" else ""
    getMaxScore(str, whTerms, getEntailmentScore(entailmentKey)(_, _, None, None))
  }

  private val semicolonSep = ";".r
  private def splitStemKeywordTokenizeFilter(text: String): Seq[Seq[String]] = {
    for {
      str <- semicolonSep.split(text).toSeq
      trimmedStr = str.trim
      if !trimmedStr.startsWith("[") || !trimmedStr.endsWith("]") // ignore strings like "[...]"
    } yield tokenizer.stemmedKeywordTokenize(trimmedStr)
  }
  // set of words that should be ignored for entailment calculation if they are the hypothesis;
  // note that in WordNet, consumer -> person -> causal_agent -> cause !
  // Additional candidates: matter, substance, whole, part, cause, unit, event, relation
  private val ignoreHypothesisSet = Set("object", "measure", "part")
  private val separator = "|||*|||"
  private val ctx1 = "ctx1="
  private val ctx2 = "ctx2="
  private def getEntailmentScore(keyPrefix: String)(text1: String, text2: String,
    context1Opt: Option[String], context2Opt: Option[String]): Double = {
    val contextKey = if (useContextInRedisCaching) {
      Seq(context1Opt.getOrElse(""), context2Opt.getOrElse("")).mkString(separator)
    } else {
      ""
    }
    val key = keyPrefix + separator + text1 + separator + text2 + separator + contextKey
    // If Redis cache is being used and contains 'key', return the stored value; otherwise
    // compute the score and, if Redis is being use, save it as the value for 'key'
    val score = redisOpt.flatMap(_.get(key)) match {
      case Some(value) => value.toDouble
      case None => {
        val text1StemmedTokens = splitStemKeywordTokenizeFilter(text1)
        val text2StemmedTokens = splitStemKeywordTokenizeFilter(text2)
        val scores = for {
          text1Seq <- text1StemmedTokens
          text2Seq <- text2StemmedTokens
          if text1Seq == text2Seq || !ignoreHypothesisSet.contains(text2Seq.mkString(" ")
            .toLowerCase)
        } yield AlignmentFunction.entailment.entail(text1Seq, text2Seq).confidence
        val scoreMax = if (scores.nonEmpty) scores.max else 0d
        redisOpt.foreach(_.set(key, scoreMax))
        scoreMax
      }
    }
    score - entailmentScoreOffset
  }
}

// cosine distance between two pieces of text (inherently symmetric)
private class Word2VecSimilarity extends SimilarityType {
  def scoreTitleTitle(text1: String, text2: String, context1Opt: Option[String],
    context2Opt: Option[String]): Double = getWord2VecScore(text1, text2)
  def scoreCellCell(text1: String, text2: String, context1Opt: Option[String],
    context2Opt: Option[String]): Double = getWord2VecScore(text1, text2)
  def scoreCellQCons(text1: String, text2: String, context1Opt: Option[String],
    context2Opt: Option[String]): Double = getWord2VecScore(text1, text2)
  def scoreTitleQCons(text1: String, text2: String, context1Opt: Option[String],
    context2Opt: Option[String]): Double = getWord2VecScore(text1, text2)
  def scoreCellQChoice(text1: String, text2: String, context1Opt: Option[String]): Double = getWord2VecScore(text1, text2)
  def scoreTitleQChoice(text1: String, text2: String, context1Opt: Option[String]): Double = getWord2VecScore(text1, text2)
  def scoreStrToWhTerms(text1: String, text2Seq: Seq[String]): Double = {
    getMaxScore(text1, text2Seq, getWord2VecScore)
  }

  private val word2vecFile = new File(
    "main/resources/vectors/GoogleNews-vectors-negative300_size=200000.bin"
  )
  private val w2vModel = Word2VecModel.fromBinFile(word2vecFile)
  private val w2vNoMatchStr = "</s>" // string used by word2vec when there is no match
  private def getWord2VecScore(text1: String, text2: String): Double = {
    val text1Modified = if (w2vModel.forSearch().contains(text1)) text1 else w2vNoMatchStr
    val text2Modified = if (w2vModel.forSearch().contains(text2)) text2 else w2vNoMatchStr
    w2vModel.forSearch().cosineDistance(text1Modified, text2Modified)
  }
}
// what fraction of text2 words are "covered" by text1 words? (directional)
private class WordOverlapSimilarity(tokenizer: KeywordTokenizer) extends SimilarityType {
  def scoreTitleTitle(text1: String, text2: String, context1Opt: Option[String], context2Opt: Option[String]): Double = {
    getSymmetricScore(text1, text2, getWordOverlap)
  }
  def scoreCellCell(text1: String, text2: String, context1Opt: Option[String], context2Opt: Option[String]): Double = {
    getSymmetricScore(text1, text2, getWordOverlap)
  }
  def scoreCellQCons(text1: String, text2: String, context1Opt: Option[String], context2Opt: Option[String]): Double = getWordOverlap(text2, text1)
  def scoreTitleQCons(text1: String, text2: String, context1Opt: Option[String], context2Opt: Option[String]): Double = getWordOverlap(text2, text1)
  def scoreCellQChoice(text1: String, text2: String, context1Opt: Option[String]): Double = getWordOverlap(text1, text2)
  def scoreTitleQChoice(text1: String, text2: String, context1Opt: Option[String]): Double = getWordOverlap(text1, text2)
  def scoreStrToWhTerms(text1: String, text2Seq: Seq[String]): Double = {
    getMaxScore(text1, text2Seq, getWordOverlap)
  }

  private def getWordOverlap(text1: String, text2: String): Double = {
    val text1StemmedTokens = tokenizer.stemmedKeywordTokenize(text1)
    val text2StemmedTokens = tokenizer.stemmedKeywordTokenize(text2)
    if (text2StemmedTokens.nonEmpty) {
      val coverage = text2StemmedTokens.intersect(text1StemmedTokens).size
      coverage.toDouble / text2StemmedTokens.size
    } else {
      0d
    }
  }
}

object AlignmentFunction {
  lazy val rootConfig = ConfigFactory.systemProperties.withFallback(ConfigFactory.load())
  lazy val localConfig = rootConfig.getConfig("entailment")
  lazy val entailer = Entailer(localConfig)
//  lazy val localEntailer = new LocalEntailer(localConfig)
//  lazy val keywordTokenizer = KeywordTokenizer.Default
//  val system = ActorSystem("ari-tableilp-trainer")
//  lazy val entailmentService = new EntailmentService(localEntailer, keywordTokenizer, system)
//  lazy val aligner = new AlignmentFunction("Entailment", Some(entailmentService), 0.1,
//    keywordTokenizer, useRedisCache = true, useContextInRedisCache = false)
  lazy val entailment = new LightEntailment(entailer)
}

class LightEntailment(entailer: org.allenai.entailment.Entailer) {
  val minEntailment = new Entailment(-1.0, Seq.empty)
  def entail(text: Seq[String], hypothesis: Seq[String]): Entailment = {
    val textTokens = toPostaggedTokens(text)
    val hypothesisTokens = toPostaggedTokens(hypothesis)
    val score = entailer.entail(textTokens, hypothesisTokens, None)
    if(score.confidence.isNaN) minEntailment else score
  }

  /** Converts a tokenized string to the PostaggedToken object entailment requires. This uses the
    * special "Any" POS tag, which entailment will ignore (it treats the token as being of any
    * possible part-of-speech). Offsets are computed assuming one character between tokens.
    */
  def toPostaggedTokens(tokens: Seq[String]): Seq[PostaggedToken] = {
    val offsets = tokens.foldLeft(Seq(0))((offsets, token) => offsets ++ Seq(offsets.last +
      token.length + 1))
    tokens zip offsets map {
      case (token, offset) => PostaggedToken(Token(token, offset), Postag.Any)
    }
  }

}

