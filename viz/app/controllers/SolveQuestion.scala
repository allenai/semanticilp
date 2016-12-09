package controllers

import javax.inject._

import models.StaticContent
import play.api._
import play.api.data.Form
import play.api.libs.json.JsNumber
import play.api.mvc._

/** This controller creates an `Action` to handle HTTP requests to the
  * application's home page.
  */
@Singleton
class SolveQuestion @Inject() extends Controller {

  /** Create an Action to render an HTML page with a welcome message.
    * The configuration in the `routes` file means that this method
    * will be called when the application receives a `GET` request with
    * a path of `/`.
    */
  def index = Action {
    //    Ok(views.html.index("Your new application is ready."))
    Ok(views.html.main("")(StaticContent.initialFormContent))
  }

  //  def solver: Result = {
  //    // Get the submitted form data from the request object, and run validation.
  //    val formData: Form[StudentFormData] = Form.form(classOf[StudentFormData]).bindFromRequest
  //    if (formData.hasErrors) {
  //      // Don't call formData.get() when there are errors, pass 'null' to helpers instead.
  //      flash("error", "Please correct errors above.")
  //      badRequest(Index.render(formData, Hobby.makeHobbyMap(null), GradeLevel.getNameList, GradePointAverage.makeGPAMap(null), Major.makeMajorMap(null)))
  //    }
  //    else {
  //      // Convert the formData into a Student model instance.
  //      val student: Student = Student.makeInstance(formData.get)
  //      flash("success", "Student instance created/edited: " + student)
  //      ok(Index.render(formData, Hobby.makeHobbyMap(formData.get), GradeLevel.getNameList, GradePointAverage.makeGPAMap(formData.get), Major.makeMajorMap(formData.get)))
  //    }
//}
  /// (parse.json)
  def solve = Action(parse.json) { implicit request =>
    //val json = request.body.asJson.get
    println("What?")
    println(request)
    println(request.body)
    Ok(views.html.main("")(StaticContent.initialFormContent)) // StaticContent.getContentWithPrefilled(0))
  }

//  def getPrefilledQuestion = Action(parse.json) { implicit request =>
//    println(request.body.as[JsNumber].value)
//    Ok(views.html.main("")(StaticContent.getContentWithPrefilled(request.body.as[JsNumber].value.toInt)))
//  }

  def getPrefilledQuestion(index: Int) = Action { request =>
    Ok(views.html.main("")(StaticContent.getContentWithPrefilled(index)))
  }

}
