package org.allenai.ari.controller.questionparser

import org.allenai.nlpstack.parse.poly.core.{ ConstituencyParse, Position, PositionTree }

/** A RotationRule specifies situations in which we want to swap children of a node in a
  * constituency parse.
  *
  * Given the context of a specific parent label, a RotationRule says that we should swap
  * any pair of its children with labels `leftLabel` and `rightLabel`.
  *
  * For instance, the rule RotationRule("root", "dobj", "nsubj") says that for any node
  * labeled "root", we should swap any children labeled "dobj" and "nsubj" if the node labeled
  * "dobj" originally appears to the left of the node labeled "nsubj".
  *
  * @param parentLabel the parent node's label
  * @param leftLabel the leftward child's label
  * @param rightLabel the rightward child's label
  */
case class RotationRule(parentLabel: String, leftLabel: String, rightLabel: String)

/** A Rotator implements a set of RotationRules.
  *
  * @param rules the set of rules to implement
  */
class Rotator(rules: Set[RotationRule]) {

  /** Given a position tree, apply all rotation rules and return the new tree.
    *
    * Note: currently this only applies rotations for children of the root position
    *
    * @param parse the original constituency parse
    * @return the rotated constituency parse
    */
  def rotate(parse: PositionTree): PositionTree = {
    val children: Seq[Position] = parse.getChildren(Position.root)
    val childPairs: Seq[(Position, Position)] = for {
      iter <- Range(0, children.size)
      child <- Range(0, children.size - 1)
    } yield (children(child), children(child + 1))
    childPairs.foldRight(parse) { (childPair: (Position, Position), parseSoFar: PositionTree) =>
      if (rules.contains(
        RotationRule(
          parseSoFar.getLabel(
          Position.root,
          ConstituencyParse.constituencyLabelName
        ).get.toLowerCase,
          parseSoFar.getLabel(
          childPair._1,
          ConstituencyParse.constituencyLabelName
        ).get.toLowerCase,
          parseSoFar.getLabel(
          childPair._2,
          ConstituencyParse.constituencyLabelName
        ).get.toLowerCase
        )
      )) {
        parseSoFar.swapPositions(childPair._1, childPair._2)
      } else {
        parseSoFar
      }
    }
  }
}
