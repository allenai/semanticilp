package org.allenai.ari.solvers.textilp.alignment.entailers

import org.allenai.entailment.{ Entailer => EntailerImpl }
import org.allenai.entailment.interface.Entailment
import org.allenai.nlpstack.core.PostaggedToken

import com.google.inject.name.Named
import com.google.inject.{ Inject, Singleton }
import com.typesafe.config.Config

/** An entailer that calls a locally-loaded entailment service. This takes up about 600M of extra
  * memory. If loading for the first time, it takes roughly a minute, plus time to download source
  * file from the datastore; if loading from an on-disk cache, it takes less than ten seconds.
  *
  * @param localConfig configuration to (optionally) pass on to the entailment library
  */
@Singleton class LocalEntailer @Inject() (
    @Named("entailment.local") localConfig: Config
) extends Entailer {

  private val entailer = EntailerImpl(localConfig)

  override def entail(
    text: Seq[PostaggedToken],
    hypothesis: Seq[PostaggedToken],
    context: Option[Seq[PostaggedToken]]
  ): Entailment = entailer.entail(text, hypothesis, context)
}
