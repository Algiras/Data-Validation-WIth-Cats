import cats.ApplicativeError
import cats.data.{NonEmptyList, Validated, ValidatedNel}
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

  def validateUserVersion0(user: UserDTO): UserDTO =
    if (isValidEmail(user.email) && isValidPassword(user.password))
      user
     else
      throw new UserValidationException

  case class Email(value: String)
  object Email {
    def apply(email: String): Option[Email] =
      Some(email).filter(isValidEmail).map(new Email(_))
  }

  case class Password(value: String)
  object Password {
    def apply(password: String): Option[Password] =
      Some(password).filter(isValidPassword).map(new Password(_))
  }

  case class User(email: Email, password: Password)

  object User {
    def apply(email: Email, password: Password): User = new User(email, password)

    def fromUserDTO(user: UserDTO): Option[User] = for {
     email <- Email(user.email)
     password <- Password(user.password)
    } yield new User(email, password)
  }

  def validateUserVersion1(user: UserDTO): Option[User] = User.fromUserDTO(user)

  lazy val userError = "User validation error"
  def validateUserVersion2(user: UserDTO): Either[String, User] = User.fromUserDTO(user).toRight(userError)

  lazy val emailError = "invalid email"
  lazy val passwordError = "invalid password"
  def validateUserVersion3(user: UserDTO): Either[String, User] = (
      Email(user.email).toRight(emailError),
      Password(user.password).toRight(passwordError)
    ) match {
      case (Right(email), Right(password)) => Right(User(email, password))
      case (Left(error), Right(_))         => Left(error)
      case (Right(_), Left(error))         => Left(error)
      case (Left(e1), Left(e2))            => Left(e1 ++ e2)
    }

  def validateUserVersion4(user: UserDTO): Either[String, User] = for {
      email <- Email(user.email).toRight(emailError)
      password <- Password(user.password).toRight(passwordError)
    } yield User(email, password)

  def validateUserVersion5(user: UserDTO): Validated[String, User] = (
      Email(user.email).toValid(emailError),
      Password(user.password).toValid(passwordError)
    ).mapN(User(_, _))

  sealed trait UserError
  final case object PasswordValidationError extends UserError

  sealed trait EmailError extends UserError
  final case object InvalidEmailError extends EmailError
  final case object BlackListedUserError extends EmailError

  lazy val blackListedUsers = Seq("bart@simsom.com")

  private def validateEmailAndEvilness(email: Email): ValidatedNel[UserError, Email] =
    Validated.condNel(!blackListedUsers.contains(email.value), email, BlackListedUserError)


  def validateUserVersion6(user: UserDTO): ValidatedNel[UserError, User] = (
      Email(user.email)
        .toValidNel(InvalidEmailError).andThen(validateEmailAndEvilness),
      Password(user.password).toValidNel(PasswordValidationError)
    ).mapN(User(_, _))

  def validateUserVersion7[F[_], E](implicit ev: ApplicativeError[F, E],
                               evTransform: NonEmptyList[UserError] => E): UserDTO => F[User] = user =>
    ev.fromValidated((
        Email(user.email).toValidNel(InvalidEmailError)
          .andThen(validateEmailAndEvilness),
        Password(user.password).toValidNel(PasswordValidationError)
      ).mapN(User(_, _)).leftMap(evTransform))
}
