package org.allenai.ari.solvers.textilp.alignment

import akka.actor.ActorSystem
import com.google.inject.{ Inject, Singleton }
import org.allenai.ari.solvers.textilp.alignment.entailers.Entailer
import org.allenai.common.Logging
import org.allenai.entailment.interface.{ Entailment, IsaResult, Postag, SynonymRelation }
import org.allenai.nlpstack.core.{ PostaggedToken, Token }
import spray.caching.{ Cache, LruCache }

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/** Service providing textual entailment for use over knowledge base ISA statements. For
  * general-purpose entailment, use `Entailer` directly.
  */
@Singleton class EntailmentService @Inject() (
  entailer: Entailer,
  tokenizer: KeywordTokenizer,
  actorSystem: ActorSystem
) extends Logging {

  import actorSystem.dispatcher

  private val entailmentCache: Cache[Entailment] =
    LruCache(initialCapacity = 10000, maxCapacity = 10000)

  /** Returns true if the given string (text or hypothesis) represents a variable. We consider
    * it a variable if it has only one token and it starts with a '?'.
    */
  private def isVariable(textOrHypothesis: Seq[String]): Boolean = {
    textOrHypothesis.length == 1 && isVariable(textOrHypothesis.head)
  }

  /** Returns true if the given token represents a variable. We consider it a variable if it starts
    * with a '?'.
    */
  private def isVariable(token: String): Boolean = token.startsWith("?")

  /** @return a cache key for the given entailment */
  private def entailmentKey(text: Seq[String], hypothesis: Seq[String]) = {
    val textKey = text.mkString("|")
    val hypothesisKey = hypothesis.mkString("|")
    s"entails($textKey, $hypothesisKey)"
  }

  /** @return a cache key for the given entailment */
  private def postaggedEntailmentKey(text: Seq[PostaggedToken], hypothesis: Seq[PostaggedToken]) = {
    val textKey = text.mkString("|")
    val hypothesisKey = hypothesis.mkString("|")
    s"entails($textKey, $hypothesisKey)"
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

  /** Entails two texts through the entailment service, first looking them up in a cache. This uses
    * the POS tag "Any" for all of the strings, and treats each string as a token. If either of
    * `text` or `hypothesis` is empty, this will return an empty entailment.
    */
  def entail(text: Seq[String], hypothesis: Seq[String]): Entailment = {
    if (text.isEmpty || hypothesis.isEmpty) {
      // Note that normally (in traditional logic) an empty hypothesis should be entailed by
      // anything. This handling of empty hypothesis was done to improve our scores.
      // TODO(jkinkead): Revisit this - it's probably better if any special-casing of emptiness was
      // moved to the caller, instead of having a non-standard entailment API.
      logger.trace(s"Empty entailment of [$text] => [$hypothesis]")
      EntailmentService.EmptyEntailment
    } else if (isVariable(text) || isVariable(hypothesis)) {
      logger.trace(s"Entailment against variable: [$text] => [$hypothesis]")
      // Fake a single-token entailment.
      val textToken = toPostaggedTokens(Seq(text.mkString("_"))).head
      val hypothesisToken = toPostaggedTokens(Seq(hypothesis.mkString("_"))).head
      // Fake an entailment using the special variable score.
      val relation =
        SynonymRelation(EntailmentService.VariableEntailmentScore, "[variable entailment]")
      val result = IsaResult(
        textToken,
        hypothesisToken,
        EntailmentService.VariableEntailmentScore,
        Seq(relation)
      )
      // Total score is the single fake entailment score.
      Entailment(EntailmentService.VariableEntailmentScore, Seq(result))
    } else {
      val cacheKey = entailmentKey(text, hypothesis)
      val futureRes = entailmentCache(cacheKey) {
        val textTokens = toPostaggedTokens(text.filterNot(isVariable))
        val hypothesisTokens = toPostaggedTokens(hypothesis.filterNot(isVariable))
        val entailment = entailer.entail(textTokens, hypothesisTokens, None)
        logger.trace(s"Entailment of [$text] => [$hypothesis]: ${entailment.confidence}")
        entailment
      }
      Await.result(futureRes, Duration.Inf)
    }
  }

  /** Entails two postagged texts through the entailment service, first looking them up in a cache.
    * If either of `text` or `hypothesis` is empty, this will return an empty entailment.
    */
  def postaggedEntail(text: Seq[PostaggedToken], hypothesis: Seq[PostaggedToken]): Entailment = {
    if (text.isEmpty || hypothesis.isEmpty) {
      // Note that normally (in traditional logic) an empty hypothesis should be entailed by
      // anything. This handling of empty hypothesis was done to improve our scores.
      logger.trace(s"Empty entailment of [$text] => [$hypothesis]")
      EntailmentService.EmptyEntailment
    } else {
      val cacheKey = postaggedEntailmentKey(text, hypothesis)
      val futureRes = entailmentCache(cacheKey) {
        val entailment = entailer.entail(text, hypothesis, None)
        logger.trace(s"Entailment of [$text] => [$hypothesis]: ${entailment.confidence}")
        entailment
      }
      Await.result(futureRes, Duration.Inf)
    }
  }

  /** Return the max entailment score between the texts and hypotheses. We consider pairwise all
    * possible combinations of texts and hypotheses, and return the highest confidence found among
    * them.
    */
  def maxEntails(texts: Seq[Seq[String]], hypotheses: Seq[Seq[String]]): Double = {
    logger.trace(s"Finding maximum entailment between ${texts.toList} and ${hypotheses.toList}")
    if (texts.isEmpty || hypotheses.isEmpty) {
      0.0
    } else {
      // Note that this loop relies on cache hits for performance; else, we'll be making a bunch of
      // redundent entailment calls.
      // TODO(jkinkead): Evaluate performance using pre-built cache keys & PostaggedToken object.
      val confidences = for {
        text <- texts
        hypothesis <- hypotheses
      } yield entail(text, hypothesis).confidence
      confidences.max
    }
  }
}

object EntailmentService {
  val EmptyEntailment = Entailment(0.0, Seq.empty)

  /** Hard-coded entailment score for variable entailment. A string is considered a variable if it's
    * a single token that starts with a '?', and will entail any other string (or be entailed to any
    * other string) with this score.
    *
    * This is set to just below 1.0, to indicate it's a very strong entailment, but not as good as a
    * perfect entailment (identity).
    */
  val VariableEntailmentScore = 0.99
}
