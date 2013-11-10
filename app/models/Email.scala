package models

object Email {
  /** Validate if a given e-mail is syntactically valid.
    *
    * @param email  E-mail address string.
    * @return       Was the e-mail address syntactically valid?
    */
  def valid(email: String): Boolean = {
    email.matches("""[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?""")
  }
}