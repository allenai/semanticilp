package org.allenai.ari.solvers.squad

import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{ Constituent, TextAnnotation, View }

import scala.util.matching.Regex

import scala.collection.JavaConverters._

/*
sealed trait PatternCons {}
sealed trait Quantifier extends PatternCons {}
case class ?(min: Int = 1, max: Int = 1) extends Quantifier
case object ? extends Quantifier
case object * extends Quantifier
case class Regex(pattern: Regex) extends PatternCons
case class RawString(str: String) extends PatternCons
case class ViewCons(label: String, surface: String, viewName: String) extends PatternCons
case class Lemma(override val label: String, override val surface: String) extends ViewCons(label, surface, ViewNames.LEMMA)
case class Chunk(override val label: String, override val surface: String) extends ViewCons(label, surface, ViewNames.SHALLOW_PARSE)
case class Pos(override val label: String, override val surface: String) extends ViewCons(label, surface, ViewNames.POS)
case class NerConll(override val label: String, override val surface: String) extends ViewCons(label, surface, ViewNames.NER_CONLL)
case class NerOntonotes(override val label: String, override val surface: String) extends ViewCons(label, surface, ViewNames.NER_ONTONOTES)
*/

object TextAnnotationPatternExtractor {
  def extractStringSequence(ta: TextAnnotation, vuName: String): (Seq[String], Seq[(Int, Int)]) = {
    val vu = ta.getView(vuName)
    val tokenSize = ta.getTokens.length
    val cons = vu.getConstituents.asScala
    cons.map { c => c.getLabel -> (c.getStartSpan, c.getEndSpan) }
    val labels = (0 until tokenSize).map { idx =>
      val cList = vu.getConstituentsCoveringToken(idx).asScala
      require(cList.length <= 1)
      if (cList.nonEmpty) {
        val c = cList.head
        //c.getLabel -> (c.getStartSpan, c.getEndSpan)
        c.getLabel -> (c.getStartSpan, c.getEndSpan)
      } else {
        ta.getToken(idx) -> (idx, idx + 1)
      }
    }.distinct
    labels.unzip
  }
  /*
  def findPatternInAnnotateedDoc(ta: TextAnnotation, pattern: Seq[PatternCons]): Seq[Seq[Int]] = {

    val requiredViews = pattern.map {
      case _: Regex => ViewNames.TOKENS
      case _: RawString => ViewNames.TOKENS
      case x: ViewCons => x.viewName
      case _ => throw new Exception("Type not found . . . ")
    }.toSet

    val seqPerViews = requiredViews.map(vu => vu -> extractStringSequence(ta, vu)).toMap


    def nextPattern(lt: Map[String, (Seq[String], Seq[(Int, Int)])], ps: List[PatternCons]): Seq[Seq[Int]] = {
      ps match {
        //if only have "*" should return all
        case List(*) =>
          val (_, tokIds) = lt(ViewNames.TOKENS)
          List(tokIds.map(_._1))
        //filter whether first str match head, if not return None
       case List(head) =>
          lt.filter(_.nonEmpty).filter(_.head._1 == head).map(r => {
            List(r.head)
          })
        //minimum match for wildcard for first str
        case * :: List(last) =>
          lt.filter(_.nonEmpty).flatMap(t => {
            t.find(_._1 == last).map(i => {
              t.takeWhile(_._1 != last) :+ i
            })
          })
        case * :: last :: l =>
          nextPattern(lt, List("*", last)).flatMap(j => {
            nextPattern(lt.map(_.drop(j.size)), l).map(i => {
              j ++ i
            })
          })
        //skip fist str
        case ? :: l =>
          lt.filter(_.nonEmpty).flatMap(r => {
            nextPattern(Some(r.tail), l).map(j => {
              r.head :: j
            })
          })
        //match the list first str
        case head :: l =>
          lt.filter(_.nonEmpty).filter(_.head._1 == head).flatMap(r => {
            nextPattern(Some(r.tail), l).map(j => {
              r.head :: j
            })
          })
      }
    }
    //if any is empty, return None
    seqPerViews(ViewNames.TOKENS)._1.isEmpty || pattern.isEmpty match {
      case true => List.empty
      case false =>
        // first find the type of the head pattern
        // and match it with the corresponding view
        pattern.head match {
          case _: Regex =>
            seqPerViews(ViewNames.TOKENS)._1
          case _: RawString => ViewNames.TOKENS
          case x: ViewCons => x.viewName
          case _ => throw new Exception("Type not found . . . ")
        }



        val relevantIndices = list.zipWithIndex.filter(_._1 == pattern.head).map(_._2)
        val relevantSublists = relevantIndices.map(list.zipWithIndex.drop)
        relevantSublists.map{ sublist =>
          nextPattern(Some(sublist), pattern).map(_.map(_._2))
        }.filter(_.isDefined).map(_.get)
    }
  }
*/
  def findPattern(list: List[String], pattern: List[String]): List[List[Int]] = {
    def nextPattern(lt: Option[List[(String, Int)]], ps: List[String]): Option[List[(String, Int)]] = {
      ps match {
        //if only have "*" should return all
        case List("*") => lt
        //filter whether first str match head, if not return None
        case List(head) =>
          lt.filter(_.nonEmpty).filter(_.head._1 == head).map(r => {
            List(r.head)
          })
        //minimum match for wildcard for first str
        case "*" :: List(last) =>
          lt.filter(_.nonEmpty).flatMap(t => {
            t.find(_._1 == last).map(i => {
              t.takeWhile(_._1 != last) :+ i
            })
          })
        case "*" :: last :: l =>
          nextPattern(lt, List("*", last)).flatMap(j => {
            nextPattern(lt.map(_.drop(j.size)), l).map(i => {
              j ++ i
            })
          })
        //skip fist str
        case "?" :: l =>
          lt.filter(_.nonEmpty).flatMap(r => {
            nextPattern(Some(r.tail), l).map(j => {
              r.head :: j
            })
          })
        //match the list first str
        case head :: l =>
          lt.filter(_.nonEmpty).filter(_.head._1 == head).flatMap(r => {
            nextPattern(Some(r.tail), l).map(j => {
              r.head :: j
            })
          })
      }
    }
    //if any is empty, return None
    list.isEmpty || pattern.isEmpty match {
      case true => List.empty
      case false =>
        val relevantIndices = list.zipWithIndex.filter(_._1 == pattern.head).map(_._2)
        val relevantSublists = relevantIndices.map(list.zipWithIndex.drop)
        relevantSublists.map { sublist =>
          nextPattern(Some(sublist), pattern).map(_.map(_._2))
        }.filter(_.isDefined).map(_.get)
    }
  }

  val posPatterns = Seq(
    Seq("JJ", "CC", "JJ"),
    Seq("JJ", "NN"),
    Seq("JJ", "NN", "NN"),
    Seq("NN", "NNS"),
    Seq("NN", "NNS", "CC", "NN"),
    Seq("NNP", "CC", "NNP"),
    Seq("NN", "CC", "NNP"),
    Seq("NN", "CC", "NNP", "NNP"),
    Seq("NN", "CC", "NNS"),
    Seq("VBG", "IN", "DT", "NN", "PRP", "VB", ",", "VB", "CC", "VB", "NN"),
    Seq("NN", "CC", "NNP", "NNP"),
    Seq("NNP", "NNP", "CC", "NNP"),
    Seq("JJ", "NN"),
    Seq("JJ", "NNP"),
    Seq("NNP", "NNS"),
    Seq("NN", "CD", "NN", "JJ"),
    Seq("NN", "CC", "JJ", "NN")
  )

  val chunkPatterns = Seq(
    Seq("NP", "NP"),
    Seq("NP", "and", "NP"),
    Seq("NP", "PP", "NP"),
    Seq("PP", "NP", "and", "NP"),
    Seq("NP", "VP", "PP", "NP"),
    Seq("VP", "NP", "PP", "NP"),
    Seq("NP", "PP", "NP", "PP", "NP"),
    Seq("NP", "and", "NP", "PP", "NP"),
    Seq("NP", ",", "NP", ", and", "NP"),
    Seq("NP", "ADVP", "PP", "NP", "VP", "NP"),
    Seq("PP", "NP", "VP", "NP", "PP", "NP", "NP"),
    Seq("NP", "PP", "NP", ",", "NP", "and", "NP")
  )

  val posNumberPatterns = Seq(
    Seq("CD", "CC", "CD"),
    Seq("CD", "TO", "CD"),
    Seq("CD", "NN", "CD")
  )

  def extractPattern(view: View, pattern: List[String]): List[List[Constituent]] = {
    val cons = view.getConstituents.asScala
    val labels = cons.map(_.getLabel).toList
    val listOfIndices = findPattern(labels, pattern)
    listOfIndices.map(_.map(i => cons.apply(i)))
  }

  def extractPatterns(ta: TextAnnotation): Seq[String] = {
    val toks = ta.getTokens.toSeq
    val (posLabels, posLabelIndices) = extractStringSequence(ta, ViewNames.POS)
    // POS patterns
    val posResults = posPatterns.flatMap { posPtrn =>
      val indexLists = findPattern(posLabels.toList, posPtrn.toList)
      indexLists.map { indices =>
        val minIdx = posLabelIndices.apply(indices.min)._1
        val maxIdx = posLabelIndices.apply(indices.max)._2
        Some(toks.slice(minIdx, maxIdx).mkString(" "))
      }.filter(_.isDefined).map(_.get)
    }

    val (chunkLabels, chunkLabelIndices) = extractStringSequence(ta, ViewNames.SHALLOW_PARSE)
    // Chunk patterns
    val chunkResults = chunkPatterns.flatMap { chnkPtrn =>
      val indexLists = findPattern(chunkLabels.toList, chnkPtrn.toList)
      indexLists.map { indices =>
        val minIdx = chunkLabelIndices.apply(indices.min)._1
        val maxIdx = chunkLabelIndices.apply(indices.max)._2
        Some(toks.slice(minIdx, maxIdx).mkString(" "))
      }.filter(_.isDefined).map(_.get)
    }

    chunkResults ++ posResults
  }

  def extractNumberPatterns(ta: TextAnnotation): Seq[Constituent] = {
    val toks = ta.getTokens.toSeq
    val (posLabels, posLabelIndices) = extractStringSequence(ta, ViewNames.POS)
    // POS patterns
    posNumberPatterns.flatMap { posPtrn =>
      val indexLists = findPattern(posLabels.toList, posPtrn.toList)
      indexLists.map { indices =>
        val minIdx = posLabelIndices.apply(indices.min)._1
        val maxIdx = posLabelIndices.apply(indices.max)._2
        Some(new Constituent("", "", ta, minIdx, maxIdx))
      }.filter(_.isDefined).map(_.get)
    }
  }

  // what ... date
  // extract all the NP VP NP and do some post-processing
  def whatSthDate(ta: TextAnnotation): Boolean = {
    val toks = ta.getTokens.toSeq
    val (chunkLabels, chunkLabelIndices) = extractStringSequence(ta, ViewNames.SHALLOW_PARSE)
    val chunkTokens = ta.getView(ViewNames.SHALLOW_PARSE).getConstituents.asScala.map(_.getSurfaceForm)

    // example: What was the US release date for Spectre?
    val indexLists1 = findPattern(chunkLabels.toList, List("NP", "VP", "NP"))
    val pattern1 = indexLists1.exists { indices =>
      assert(indices.length == 3)
      val str = indices.map { idxPair =>
        val minIdx = chunkLabelIndices.apply(idxPair)._1
        val maxIdx = chunkLabelIndices.apply(idxPair)._2
        toks.slice(minIdx, maxIdx).mkString(" ").toLowerCase
      }
      str.head.contains("what")
      str(2).contains("date")
    }

    // example: New Amsterdam became the title of New York City in what past date?
    val indexLists2 = findPattern(chunkLabels.toList, List("NP"))
    val pattern2 = indexLists2.exists { indices =>
      assert(indices.length == 1)
      val minIdx = chunkLabelIndices.apply(indices.min)._1
      val maxIdx = chunkLabelIndices.apply(indices.max)._2
      val str = toks.slice(minIdx, maxIdx).mkString(" ").toLowerCase
      str.contains("what") && str.contains(" date")
    }

    pattern1 || pattern2
  }

}
