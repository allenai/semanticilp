package org.allenai.ari.solvers.textilp.alignment.entailers

import org.allenai.entailment.interface.{ Entailment, EntailmentRequest }
import org.allenai.nlpstack.core.PostaggedToken

import akka.actor.ActorSystem
import com.google.inject.{ Inject, Singleton }
import com.google.inject.name.Named
import spray.client.pipelining._
import spray.httpx.SprayJsonSupport

import scala.concurrent.Await
import scala.concurrent.duration._

@Singleton class RemoteEntailer @Inject() (
  @Named("entailment.remote.url") entailmentUrl: String,
  @Named("entailment.remote.timeoutMillis") timeoutMillis: Int,
  private implicit val actorSystem: ActorSystem
) extends Entailer with SprayJsonSupport {
  import actorSystem.dispatcher

  private val entailmentTimeout: Duration = timeoutMillis.millis

  override def entail(
    text: Seq[PostaggedToken],
    hypothesis: Seq[PostaggedToken],
    context: Option[Seq[PostaggedToken]]
  ): Entailment = {
    // create EntailmentRequest(text, hypothesis, context, combinationMode, algorithms)
    val entailmentRequest = EntailmentRequest(text, hypothesis, context, None, None)
    val pipeline = sendReceive ~> unmarshal[Entailment]
    val request = pipeline(Post(entailmentUrl, entailmentRequest))
    Await.result(request, entailmentTimeout)
  }
}
