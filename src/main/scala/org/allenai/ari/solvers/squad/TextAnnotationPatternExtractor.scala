package org.allenai.ari.solvers.squad

object TextAnnotationPatternExtractor {
  def findPattern(list: List[String], pattern: List[String]): Option[List[Int]] = {
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
      case true => None
      case false => nextPattern(Some(list.zipWithIndex.dropWhile(_._1 != pattern.head)), pattern).map(_.map(_._2))
    }
  }
}
