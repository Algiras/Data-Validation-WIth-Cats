import Main._
import cats.data.Validated.{Invalid, _}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import org.specs2.mutable.{Specification, Tables}

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

  "validateUserVersion0 validates if user is a valid user or throws" >> {
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
          validateUserVersion0(user) must_=== user
        } else {
          validateUserVersion0(user) must throwA[UserValidationException]
        }
      }
    }
  }

  "validateUserVersion1 validates if user is a valid user" >> {
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
        validateUserVersion1(user) must_=== (if (isValid) User.fromUserDTO(user) else None)
      }
    }
  }

  "validateUserVersion2 validates if user is a valid user or returns Left error" >> {
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
        validateUserVersion2(user) must_=== (if (isValid) User.fromUserDTO(user).toRight(???) else Left(userError))
      }
    }
  }

  "validateUserVersion3 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                |>
      "ak@email.com"       !(null: String)    !! Some(passwordError)                    |
      (null: String)       !"asdfga"          !! Some(emailError)                       |
      (null: String)       !(null: String)    !! Some(emailError ++ passwordError)      |
      "ak@email.com"       !"asdf12"          !! None                                   |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)
        validateUserVersion3(user) must_=== (if (error.isEmpty) User.fromUserDTO(user).toRight(???)
                                        else Left(error.get))
      }
    }
  }

  "validateUserVersion4 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                |>
      "ak@email.com"       !(null: String)    !! Some(passwordError)                    |
      (null: String)       !"asdfga"          !! Some(emailError)                       |
      (null: String)       !(null: String)    !! Some(emailError)                       |
      "ak@email.com"       !"asdf12"          !! None                                   |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)
        validateUserVersion4(user) must_=== (if (error.isEmpty) User.fromUserDTO(user).toRight(???)
                                        else Left(error.get))
      }
    }
  }

  "validateUserVersion5 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                |>
      "ak@email.com"       !(null: String)    !! Some(passwordError)                    |
      (null: String)       !"asdfga"          !! Some(emailError)                       |
      (null: String)       !(null: String)    !! Some(emailError ++ passwordError)      |
      "ak@email.com"       !"asdf12"          !! None                                   |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)
        validateUserVersion5(user) must_=== (if (error.isEmpty) User.fromUserDTO(user).toValid(???)
                                        else Invalid(error.get))
      }
    }
  }

  "validateUserVersion6 validates if user is a valid user or returns Left error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                                                      |>
      "ak@email.com"       !(null: String)    !! Some(NonEmptyList.of(PasswordValidationError))                               |
      (null: String)       !"asdfga"          !! Some(NonEmptyList.of(InvalidEmailError))                                     |
      (null: String)       !(null: String)    !! Some(NonEmptyList(InvalidEmailError, List(PasswordValidationError)))         |
      "bart@simsom.com"    !"asdf12"          !! Some(NonEmptyList.of(BlackListedUserError))                                  |
      "ak@email.com"       !"asdf12"          !! None                                                                         |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)
        validateUserVersion6(user) must_=== (if (error.isEmpty) User.fromUserDTO(user).toValid(???)
                                        else Invalid(error.get))
      }
    }
  }

  "validateUserVersion7 validates if user is a valid user or returns an error" >> {
    // @formatter:off
      "email"              | "password"       || "error"                                                                 |>
      "ak@email.com"       !(null: String)    !! Some(NonEmptyList.of(PasswordValidationError))                          |
      (null: String)       !"asdfga"          !! Some(NonEmptyList.of(InvalidEmailError))                                |
      (null: String)       !(null: String)    !! Some(NonEmptyList(InvalidEmailError, List(PasswordValidationError)))    |
      "bart@simsom.com"    !"asdf12"          !! Some(NonEmptyList.of(BlackListedUserError))                             |
      "ak@email.com"       !"asdf12"          !! None                                                                    |> {
      // @formatter:on
        case (email, password, error) => {
          val user = UserDTO(email, password)

          implicit def stringToOptError(error: NonEmptyList[UserError]): Unit = ()
          val optValidation = validateUserVersion7[Option, Unit]
          optValidation(user) must_=== (if(error.isEmpty)
            User.fromUserDTO(user)
          else None)

          val validatedValidation = validateUserVersion7[Validated[NonEmptyList[UserError], ?], NonEmptyList[UserError]]
           validatedValidation(user) must_=== (
            if(error.isEmpty)
              User.fromUserDTO(user).toValid(???)
            else
              Invalid(error.get))

          val eitherValidation = validateUserVersion7[Either[NonEmptyList[UserError], ?], NonEmptyList[UserError]]
          eitherValidation(user) must_=== (
            if(error.isEmpty)
              User.fromUserDTO(user).toRight(???)
            else
              Left(error.get)
          )
        }
      }
  }
}
