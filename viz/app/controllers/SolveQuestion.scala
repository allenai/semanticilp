package controllers

import javax.inject._

import models.StaticContent
import org.allenai.ari.solvers.textilp.solvers.{SalienceSolver, TextILPSolver}
import play.api._
import play.api.data.Form
import play.api.libs.json.{JsNumber, JsString}
import play.api.mvc._

/** This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class SolveQuestion @Inject() extends Controller {

  lazy val salienceSolver = new SalienceSolver()
  //lazy val textilpSolver = new TextILPSolver {}()

  /** Create an Action to render an HTML page with a welcome message.
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index = Action {
    //    Ok(views.html.index("Your new application is ready."))
    Ok(views.html.main("")(StaticContent.initialFormContent))
  }

  def solve = Action(parse.json) { implicit request =>
    println(request.body)
    val solverType = (request.body \ "solverType").as[JsString].value
    val question = (request.body \ "question").as[JsString].value
    val options = (request.body \ "options").as[JsString].value
    val snippet = (request.body \ "snippet").as[JsString].value

    println("solver type : " + solverType)
    val newPageContent = if(solverType.toLowerCase.contains("salience")) {
      println("Calling salience . . . ")
      val out = salienceSolver.solver(question, options.split("//").toSet, snippet)
      println("Salience solver response ..... ")
      println(out)
      StaticContent.initialFormContent.copy(solverLog = out.toString)
    }
    else {
      throw new Exception("the solver not found :/")
    }

    Ok(views.html.main("")(newPageContent))
  }

  def getPrefilledQuestion(index: Int) = Action { request =>
    Ok(views.html.main("")(StaticContent.getContentWithPrefilled(index)))
  }

}
