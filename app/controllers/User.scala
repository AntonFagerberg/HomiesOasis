package controllers

import play.api._
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms._
import AuthenticatedRequest.authenticatedRequest

object User extends Controller {
  lazy val loginForm = Form(
    tuple(
      "usernameOrEmail" -> nonEmptyText.transform(
        usernameOrEmail =>
          if (models.Email.valid(usernameOrEmail)) Left(usernameOrEmail): Either[String, String]
          else Right(usernameOrEmail): Either[String, String]
        ,
        (usernameOrEmail: Either[String, String]) =>
          usernameOrEmail match {
            case Left(emailAddress) => emailAddress
            case Right(username) => username
          }
      ),
      "password" -> nonEmptyText
    )
  )

  lazy val registrationForm = Form(
    tuple(
      "user" -> mapping(
        "id" -> ignored(-1l),
        "username" -> nonEmptyText(maxLength = 25).verifying(!models.User.usernameExists(_)),
        "email" -> nonEmptyText(maxLength = 255).verifying(email => models.Email.valid(email) && !models.User.emailExists(email))
      )(models.User.apply)(models.User.unapply),
      "password" -> nonEmptyText(minLength = 6)
    )
  )

  def login = Action {
    Ok(views.html.user.login(loginForm))
  }

  def authenticate = Action { implicit request =>
    val sentForm = loginForm.bindFromRequest()
    Ok(views.html.user.login(sentForm))
  }

  def register = Action {
    Ok(views.html.user.register(registrationForm))
  }

  def createAccount = Action { implicit request =>
    val sentForm = registrationForm.bindFromRequest()

//    if (sentForm.hasErrors) {
    models.User.create(sentForm.get._1, sentForm.get._2.toCharArray)
      BadRequest(views.html.user.register(sentForm))
//    } else if (models.User.create(sentForm.get._1, sentForm.get._2.toCharArray)) {
//        Redirect(routes.User.login).withNewSession.withSession("userId" -> userId.toString)
//    } else {
//      BadRequest(views.html.user.register(sentForm.withError("userCreation", "Unable to create user.")))
//    }
  }

  def settings = authenticatedRequest() { implicit request =>
    Ok(views.html.user.settings())
  }
}