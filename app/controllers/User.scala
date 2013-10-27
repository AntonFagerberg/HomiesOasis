package controllers

import play.api._
import play.api.mvc._

object User extends Controller {
  def login = Action {
    Ok(views.html.user.login())
  }

  def register = Action {
    Ok(views.html.user.register())
  }
}