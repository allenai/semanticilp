package org.allenai.ari.solvers.textilp.utils

import edu.illinois.cs.cogcomp.annotation.Annotator
import edu.illinois.cs.cogcomp.core.datastructures.ViewNames
import edu.illinois.cs.cogcomp.core.datastructures.textannotation.{Constituent, SpanLabelView, TextAnnotation}
import edu.illinois.cs.cogcomp.core.utilities.configuration.ResourceManager

import scala.collection.JavaConverters._

class IndependentClauseViewGenerator(val finalViewName: String) extends Annotator(finalViewName, Array(ViewNames.DEPENDENCY_STANFORD)) {
  override def initialize(rm: ResourceManager): Unit = { }

  override def addView(ta: TextAnnotation): Unit = {
    val dependencyCons = ta.getView(ViewNames.DEPENDENCY_STANFORD).getConstituents
    val newView = new SpanLabelView(finalViewName, "IndependentClauseViewGenerator", ta, 1.0)
    val tokens = ta.getView(ViewNames.DEPENDENCY_STANFORD).getConstituents.asScala
    val posView = ta.getView(ViewNames.POS)
    val verbs = Set("VB", "VBZ", "VBP")
    val ands = tokens.filter{c => c.getSurfaceForm == "and" && verbs.contains(posView.getConstituentsCovering(c.getIncomingRelations.get(0).getSource).get(0).getLabel)}
    val indices = 0 +: ands.map(_.getEndSpan) :+ ta.getTokens.length
    indices.sliding(2).foreach{ list => newView.addConstituent(new Constituent("", "", ta, list.head, list(1))) }
    ta.addView(finalViewName, newView)
  }
}
