package org.allenai.ari.solvers.textilp.alignment.entailers

import org.allenai.entailment.interface.Entailment
import org.allenai.nlpstack.core.PostaggedToken

/** Entailer responsible for calculating entailment between token sequences. This wraps the raw
  * entailment calls the service provides.
  */
trait Entailer {
  def entail(
    text: Seq[PostaggedToken],
    hypothesis: Seq[PostaggedToken],
    context: Option[Seq[PostaggedToken]]
  ): Entailment
}
