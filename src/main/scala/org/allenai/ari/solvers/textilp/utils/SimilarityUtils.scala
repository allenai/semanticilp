package org.allenai.ari.solvers.textilp.utils

object SimilarityUtils {

  import scala.math._

  object Levenshtein {
    def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)

    def distance(s1: String, s2: String) = {
      val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }
      for (j <- 1 to s2.length; i <- 1 to s1.length)
        dist(j)(i) = if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1) else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)
      dist(s2.length)(s1.length)
    }
    def printDistance(s1: String, s2: String) = println("%s -> %s : %d".format(s1, s2, distance(s1, s2)))
  }

  /** longest common subsequence */
  def lcs[T]: (List[T], List[T]) => List[T] = {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x :: xs, y :: ys) if x == y => x :: lcs(xs, ys)
    case (x :: xs, y :: ys) =>
      (lcs(x :: xs, ys), lcs(xs, y :: ys)) match {
        case (xs1, ys1) if xs1.length > ys1.length => xs1
        case (xs1, ys2) => ys2
      }
  }

  case class Memoized[A1, A2, B](f: (A1, A2) => B) extends ((A1, A2) => B) {
    val cache = scala.collection.mutable.Map.empty[(A1, A2), B]
    def apply(x: A1, y: A2) = cache.getOrElseUpdate((x, y), f(x, y))
  }

  lazy val lcsM: Memoized[List[Char], List[Char], List[Char]] = Memoized {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (x :: xs, y :: ys) if x == y => x :: lcsM(xs, ys)
    case (x :: xs, y :: ys) => {
      (lcsM(x :: xs, ys), lcsM(xs, y :: ys)) match {
        case (xs, ys) if xs.length > ys.length => xs
        case (xs, ys) => ys
      }
    }
  }

}

