package models

import javax.crypto.spec.PBEKeySpec
import javax.crypto.SecretKeyFactory
import java.security.SecureRandom
import play.api.Play.current
import play.api.db._
import anorm._
import anorm.SqlParser._

/** User account information.
  * Passwords are not stored in these objects.
  *
  * @param id       Id-number of user.
  * @param username Username of user.
  * @param email    E-mail address of user.
  */
case class User(
  id: Long,
  username: String,
  email: String
)

object User {
  val userParser =
    get[Long]("user.id") ~
    get[String]("user.username") ~
    get[String]("user.email") map { case id ~ email ~ username =>
      User(id, username, email)
    }

  /** Check if the username is already registered to a user in the database.
    * This method is typically called before creating a new user.
    *
    * @param username Username.
    * @return         Does the username exist in the database?
    */
  def usernameExists(username: String): Boolean = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          |SELECT
          | COUNT(*)
          |FROM
          | user
          |WHERE
          | user.username = {username}
          |LIMIT 1
        """.stripMargin
      ).on(
        'username -> username
      ).as(
        scalar[Long].single
      ) > 0
    }
  }

  /** Check if the e-mail address is already registered to a user in the database.
    * This method is typically called before creating a new user.
    *
    * @param email E-mail address.
    * @return      Does the username exist in the database?
    */
  def emailExists(email: String): Boolean = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          |SELECT
          | COUNT(*)
          |FROM
          | user
          |WHERE
          | user.email = {email}
          |LIMIT 1
        """.stripMargin
      ).on(
        'email -> email
      ).as(
        scalar[Long].single
      ) > 0
    }
  }

  /** Create a new User account (save it to the database).
    *
    * The Password will be hashed by the hash-function in this object before being inserted,
    * and a new random salt will be created.
    *
    * @param user     User details.
    * @param password Password clear-text.
    * @return         Was the user account created?
    */
  def create(user: User, password: Array[Char]): Unit = {
    DB.withConnection { implicit connection =>
      val salt = randomSalt

      SQL(
        """
          |INSERT INTO
          | user
          |SET
          | user.username = {username},
          | user.email = {email},
          | user.password = {password},
          | user.salt = {salt}
        """.stripMargin
      ).on(
        'username -> user.username,
        'email -> user.email,
        'password -> hash(password, salt),
        'salt -> salt
      ).executeInsert(scalar[Long].single)
    }
  }

  /** Get a user account from its id-number.
    *
    * Used primarily to bind a user object to the request when the user is signed in.
    *
    * @param id Id-number of user account.
    * @return   User if id-number was valid.
    */
  def userFromId(id: Long): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          |SELECT
          | user.id,
          | user.username,
          | user.email
          |FROM
          | user
          |WHERE
          | user.id = {id}
          |LIMIT 1
        """.stripMargin
      ).on(
        'id -> id
      ).as(
        userParser.singleOpt
      )
    }
  }

  /** Authenticate a user by validating the email or username with password.
    *
    * @param emailUsername  Either a email Left(String) or a username Right(String).
    * @param password       Password clear text (the corresponding salt from database will be used).
    * @return
    */
  def authenticate(emailUsername: Either[String, String], password: Array[Char]): Boolean = {
    DB.withConnection { implicit connection =>
      val validationUser =
        SQL(
          """
            |SELECT
            | user.password,
            | user.salt
            |FROM
            | user
            |WHERE
            | user.email = {email}
            |OR
            | user.username = {username}
            |LIMIT 1
          """.stripMargin
        ).on(
          'email -> emailUsername.left,
          'username -> emailUsername.right
        ).singleOpt().map { case Row(password: Array[Byte], salt: Array[Byte]) =>
          password -> salt
        }

      validationUser.isDefined && validationUser.get._1.sameElements(hash(password, validationUser.get._2))
    }
  }

  /** Returns byte array hash created with PBKDF2WithHmacSHA1.
    *
    * @param clearText  Text to be hashed.
    * @param salt       Salt used when hashing the clearText.
    * @return           Hashed clearText.
    */
  private def hash(clearText: Array[Char], salt: Array[Byte]): Array[Byte] = {
    // Read about these values for PBKDF2 before changing iterationCount or keyLength.
    // Beware of changing these values if there are existing users in the database (you might break them).
    val iterationCount = 20480
    val keyLength = 160

    val spec = new PBEKeySpec(clearText, salt, iterationCount, keyLength)
    SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1").generateSecret(spec).getEncoded
  }

  /** Generate a pseudo-random salt used when hashing passwords created with SHA1PRNG.
    *
    * The salt is considered a non-secret and can be stored with the passwords in the database.
    * When a password is changed, the salt can, but does not have to be modified.
    *
    * @return Pseudo-random generated salt.
    */
  private def randomSalt: Array[Byte] = {
    val numberOfBytes = 20
    val random = SecureRandom.getInstance("SHA1PRNG")
    val salt = new Array[Byte](numberOfBytes)
    random.nextBytes(salt)
    salt
  }
}