import org.specs2.mutable.{Specification, Tables}
import Main._
import cats.{Applicative, ApplicativeError}
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.instances.option._
import cats.data.Validated._
import cats.instances.string._

class MainTest extends Specification with Tables {
  "isValidEmail validates if the email is valid" >> {
    // @formatter:off
      "email"            | "is valid" |>
        "some@email.com" ! true       |
        (null: String)   ! false      |
        ""               ! false      |
        "bla"            ! false      |> {
      // @formatter:on
        case (email, isValid) => isValidEmail(email) must_=== isValid
      }
  }

  "isValidPassword validates if the password is valid" >> {
    // @formatter:off
      "password"       | "is valid" |>
        (null: String) ! false      |
        "asdfga"       ! true       |
        "asdf12"       ! true       |
        "asd@12"       ! false      |
        "bla"          ! false      |> {
      // @formatter:on
        case (password, isValid) => isValidPassword(password) must_=== isValid
      }
  }

  "validateUserLv0 validates if user is a valid user or throws" >> {
    // @formatter:off
      "email"              | "password"       || "is valid" |>
      "ak@email.com"       !(null: String)    !! false      |
      (null: String)       !"asdfga"          !! false      |
      "ak@email.com"       !"asdfga"          !! true       |
      "ak@email.com"       !"asdf12"          !! true       |
      "ak@email.com"       ! "asd@12"         !! false      |
      "akbvemail123"       ! "asd@12"         !! false      |
      "ak@email.com"       ! "bla"            !! false      |> {
        // @formatter:on
        case (email, password, isValid) => {
          val user = UserDTO(email, password)
        if (isValid) {
          validateUserLv0(user) must_=== user
        } else {
          validateUserLv0(user) must throwA[UserValidationException]
        }
      }
    }
  }

  "validateUserLv1 validates if user is a valid user" >> {
    // @formatter:off
      "email"              | "password"     || "is valid" |>
        "ak@email.com"     ! (null: String) !! false      |
        (null: String)     ! "asdfga"       !! false      |
        "ak@email.com"     ! "asdfga"       !! true       |
        "ak@email.com"     ! "asdf12"       !! true       |
        "ak@email.com"     ! "asd@12"       !! false      |
        "akbvemail123"     !"asd@12"        !! false      |
        "ak@email.com"     !"bla"           !! false      |> {
      // @formatter:on
        case (email, password, isValid) => {
          val user = UserDTO(email, password)
        validateUserLv1(user) must (if (isValid) beSome(User.fromUserDTO(user)) else beNone)
      }
    }
  }

  "validateUserLv2 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "is valid" |>
      "ak@email.com"       !(null: String)    !! false      |
        (null: String)     ! "asdfga"         !! false      |
        "ak@email.com"     ! "asdfga"         !! true       |
        "ak@email.com"     ! "asdf12"         !! true       |
        "ak@email.com"     ! "asd@12"         !! false      |
        "akbvemail123"     ! "asd@12"         !! false      |
        "ak@email.com"     ! "bla"            !! false      |> {
        // @formatter:on
        case (email, password, isValid) => {
          val user = UserDTO(email, password)
        validateUserLv2(user) must (if (isValid) beRight(User.fromUserDTO(user)) else beLeft)
      }
    }
  }

  "validateUserLv3 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                |>
      "ak@email.com"       !(null: String)    !! Some(passwordError)                    |
      (null: String)       !"asdfga"          !! Some(emailError)                       |
      (null: String)       !(null: String)    !! Some(emailError ++ passwordError)      |
      "ak@email.com"       !"asdf12"          !! None                                   |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)
        validateUserLv3(user) must_=== (if (error.isEmpty) Right(User.fromUserDTO(user))
                                        else Left(error.get))
      }
    }
  }

  "validateUserLv4 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                |>
      "ak@email.com"       !(null: String)    !! Some(passwordError)                    |
      (null: String)       !"asdfga"          !! Some(emailError)                       |
      (null: String)       !(null: String)    !! Some(emailError)                       |
      "ak@email.com"       !"asdf12"          !! None                                   |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)
        validateUserLv4(user) must_=== (if (error.isEmpty) Right(User.fromUserDTO(user))
                                        else Left(error.get))
      }
    }
  }

  "validateUserLv5 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                |>
      "ak@email.com"       !(null: String)    !! Some(passwordError)                    |
      (null: String)       !"asdfga"          !! Some(emailError)                       |
      (null: String)       !(null: String)    !! Some(emailError ++ passwordError)      |
      "ak@email.com"       !"asdf12"          !! None                                   |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)
        validateUserLv5(user) must_=== (if (error.isEmpty) Valid(User.fromUserDTO(user))
                                        else Invalid(error.get))
      }
    }
  }

  "validateUserLv6 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                             |>
      "ak@email.com"       !(null: String)    !! Some(NonEmptyList(passwordError, List.empty))       |
      (null: String)       !"asdfga"          !! Some(NonEmptyList(emailError, List.empty))          |
      (null: String)       !(null: String)    !! Some(NonEmptyList(emailError, List(passwordError))) |
      "bart@simsom.com"    !"asdf12"          !! Some(NonEmptyList(evilUserError, List.empty))       |
      "ak@email.com"       !"asdf12"          !! None                                                |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)
        validateUserLv6(user) must_=== (if (error.isEmpty) Valid(User.fromUserDTO(user))
                                        else Invalid(error.get))
      }
    }
  }

  "validateUserLv7 validates if user is a valid user or returns an error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                                                 |>
      "ak@email.com"       !(null: String)    !! Some(NonEmptyList(PasswordValidationError, List.empty))                 |
      (null: String)       !"asdfga"          !! Some(NonEmptyList(EmailValidationError, List.empty))                    |
      (null: String)       !(null: String)    !! Some(NonEmptyList(EmailValidationError, List(PasswordValidationError))) |
      "ak@email.com"       !"asdf12"          !! None                                                                    |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)

          implicit def stringToOptError(error: UserError): Unit = Unit
          validateUserLv7[Option, Unit](user) must_=== (if(error.isEmpty) Some(User.fromUserDTO(user)) else None)

          implicit def userErrorToInvalidError[T](error: UserError): NonEmptyList[UserError] = NonEmptyList(error, List.empty)
          validateUserLv7[Validated[NonEmptyList[UserError], ?], NonEmptyList[UserError]](user) must_=== (if(error.isEmpty) Valid(User.fromUserDTO(user)) else Invalid(error.get))
        }
      }
  }

  implicit def nonEmptyListError[T](implicit ev: Applicative[Validated[NonEmptyList[T], ?]]) = new ApplicativeError[Validated[NonEmptyList[T], ?], NonEmptyList[T]] {
    override def raiseError[A](e: NonEmptyList[T]): Validated[NonEmptyList[T], A] = Invalid(e)
    override def handleErrorWith[A](
        fa: Validated[NonEmptyList[T], A])(
        f: NonEmptyList[T] => Validated[
          NonEmptyList[T],
          A]): Validated[NonEmptyList[T], A] = fa match {
      case value@Valid(_) => value
      case Invalid(error) => f(error)
    }

    override def pure[A](x: A): Validated[NonEmptyList[T], A] = ev.pure(x)

    override def ap[A, B](
  ff: Validated[NonEmptyList[T],
                                       A => B])(
        fa: Validated[NonEmptyList[T], A])
      : Validated[NonEmptyList[T], B] = ev.ap(ff)(fa)
  }
}
