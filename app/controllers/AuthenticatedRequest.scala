package controllers

import play.api.mvc._

/** A request performed by an authenticated user.
  *
  * @param user     User who sent the request.
  * @param request  The request.
  */
case class AuthenticatedRequest(
  user: models.User,
  request: Request[AnyContent]
) extends WrappedRequest(request)

object AuthenticatedRequest extends Controller {
  val sessionField = "userId"

  /** User sends an authenticated request to access a resource via a Controller.
    *
    * @param authenticationFunction Authentication function to use.
    * @param requestResult          Function request.
    * @return                       Action or Redirect.
    */
  def authenticatedRequest(authenticationFunction: Long => Option[models.User] = models.User.userFromId)(requestResult: AuthenticatedRequest => SimpleResult) = {
    Action { request =>
      request.session.get(sessionField).flatMap(idNumber => authenticationFunction(idNumber.toLong)).map { user =>
        requestResult(AuthenticatedRequest(user, request))
      } getOrElse {
        Redirect(routes.User.login).flashing("warning" -> "permissionDenied")
      }
    }
  }
}