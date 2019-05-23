import cats.ApplicativeError
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._

import scala.language.higherKinds

object Main extends App {
  class UserValidationException extends Exception("User validation exception")

  case class UserDTO(email: String, password: String)

  lazy private val emailRegex =
    """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
  lazy private val passwordRegex = """[a-zA-Z0-9]+"""

  def isValidEmail(email: String): Boolean = email match {
    case null                                          => false
    case e if e.trim.isEmpty                           => false
    case e if emailRegex.findFirstMatchIn(e).isDefined => true
    case _                                             => false
  }

  def isValidPassword(password: String): Boolean = password match {
    case null                                          => false
    case p if p.trim.isEmpty                           => false
    case p if p.matches(passwordRegex) && p.length > 5 => true
    case _                                             => false
  }

  def validateUserLv0(user: UserDTO): UserDTO = {
    if (isValidEmail(user.email) && isValidPassword(user.password)) {
      user
    } else {
      throw new UserValidationException
    }
  }

  case class Email(value: String)
  case class Password(value: String)
  case class User(email: Email, password: Password)

  object User {
    def fromUserDTO(user: UserDTO): User =
      User(Email(user.email), Password(user.password))
  }

  def validateUserLv1(user: UserDTO): Option[User] = {
    if (isValidEmail(user.email) && isValidPassword(user.password)) {
      Some(User.fromUserDTO(user))
    } else {
      None
    }
  }

  lazy val userError = "User validation error"
  def validateUserLv2(user: UserDTO): Either[String, User] = {
    if (isValidEmail(user.email) && isValidPassword(user.password)) {
      Right(User.fromUserDTO(user))
    } else {
      Left(userError)
    }
  }

  lazy val emailError = "invalid email"
  lazy val passwordError = "invalid password"
  def validateUserLv3(user: UserDTO): Either[String, User] = {
    def checkUserField[T](getValue: UserDTO => String,
                          isValid: String => Boolean,
                          toT: String => T,
                          error: String)(user: UserDTO) = {
      val value = getValue(user)
      if (isValid(value)) {
        Right(toT(value))
      } else Left(error)
    }

    (checkUserField(_.email, isValidEmail, Email, emailError)(user),
     checkUserField(_.password, isValidPassword, Password, passwordError)(user)) match {
      case (Right(email), Right(password)) => Right(User(email, password))
      case (Left(error), Right(_))         => Left(error)
      case (Right(_), Left(error))         => Left(error)
      case (Left(e1), Left(e2))            => Left(e1 ++ e2)
    }
  }

  def validateUserLv4(user: UserDTO): Either[String, User] = {
    def checkUserField[T](getValue: UserDTO => String,
                          isValid: String => Boolean,
                          toT: String => T,
                          error: String)(user: UserDTO) = {
      val value = getValue(user)
      if (isValid(value)) {
        Right(toT(value))
      } else Left(error)
    }

    for {
      email <- checkUserField(_.email, isValidEmail, Email, emailError)(user)
      password <- checkUserField(_.password, isValidPassword, Password, passwordError)(user)
    } yield User(email, password)
  }

  def validateUserLv5(user: UserDTO): Validated[String, User] = {
    def checkUserField[T](getValue: UserDTO => String,
                          isValid: String => Boolean,
                          toT: String => T,
                          error: String)(user: UserDTO) = {
      val value = getValue(user)
      if (isValid(value)) {
        Valid(toT(value))
      } else Invalid(error)
    }

    (checkUserField(_.email, isValidEmail, Email, emailError)(user),
     checkUserField(_.password, isValidPassword, Password, passwordError)(user))
      .mapN((email, password) => User(email, password))
  }

  lazy val evilUserError = "provided email is evil"
  lazy val evilEmails = Seq("bart@simsom.com")
  private def validateEmailAndEvilness(validEmail: Validated[NonEmptyList[String], Email])
    : Validated[NonEmptyList[String], Email] =
    validEmail.andThen(email =>
      if (evilEmails.contains(email.value)) {
        Invalid(NonEmptyList(evilUserError, List.empty))
      } else {
        Valid(email)
      })

  def validateUserLv6(user: UserDTO): Validated[NonEmptyList[String], User] = {
    def checkUserField[T](getValue: UserDTO => String,
                          isValid: String => Boolean,
                          toT: String => T,
                          error: NonEmptyList[String])(user: UserDTO) = {
      val value = getValue(user)
      if (isValid(value)) {
        Valid(toT(value))
      } else Invalid(error)
    }

    val validEmail = validateEmailAndEvilness(
      checkUserField(_.email, isValidEmail, Email, NonEmptyList(emailError, List.empty))(user)
    )

    (validEmail,
     checkUserField(_.password, isValidPassword, Password, NonEmptyList(passwordError, List.empty))(user))
      .mapN((email, password) => User(email, password))
  }

  sealed trait UserError
  final case object EmailValidationError extends UserError
  final case object PasswordValidationError extends UserError

  def validateUserLv7[F[_], E](user: UserDTO)(
      implicit ev: ApplicativeError[F, E],
      evTransform: UserError => E): F[User] = {
    def checkUserField[T](getValue: UserDTO => String,
                          isValid: String => Boolean,
                          toT: String => T,
                          error: UserError)(user: UserDTO): F[T] = {
      val value = getValue(user)
      if (isValid(value)) {
        ev.pure(toT(value))
      } else ev.raiseError(evTransform(error))
    }

    (checkUserField(_.email, isValidEmail, Email, EmailValidationError)(user),
     checkUserField(_.password, isValidPassword, Password, PasswordValidationError)(user)
    ).mapN((email: Email, password: Password) => User(email, password))
  }
}
