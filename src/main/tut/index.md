
class: center, middle

# Data Validation with Cats

**By:** Algimantas Krasauskas

**From:** Wix People Team

![cato](images/cato.gif "Cato")

---
# The Problem

We got a requirement to validate user data coming from a Front End Registration form

```tut:silent
case class UserDTO(email: String, password: String)

def isValidEmail(email: String): Boolean = ???
def isValidPassword(password: String): Boolean = ???

```
---
# Version 0 Naive Implementation

Naive implementation that just throws, when validation fails

```tut:silent
class UserValidationException extends Exception("User validation exception")

def validateUserVersion0(user: UserDTO): UserDTO =
  if (isValidEmail(user.email) && isValidPassword(user.password)) {
    user
  } else {
    throw new UserValidationException
  }
```

---
# Version 0 Naive Implementation

<img src="images/boom.gif" style="display: block; margin-left: auto;margin-right: auto; width: 75%;"/>

---
# Version 1 Smart Constructors

```tut:silent
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
```

---
#  Version 2 Transforming to Either

```tut:silent
val userError = "User validation error"
def validateUserVersion2(user: UserDTO): Either[String, User] = 
    User.fromUserDTO(user).toRight(userError)

```
---
# Version 3 Combining Errors

```tut:silent
val emailError = "invalid email"
val passwordError = "invalid password"

def validateUserVersion3(user: UserDTO): Either[String, User] = (
    Email(user.email).toRight(emailError),
    Password(user.password).toRight(passwordError)
  ) match {
    case (Right(email), Right(password)) => Right(User(email, password))
    case (Left(error), Right(_))         => Left(error)
    case (Right(_), Left(error))         => Left(error)
    case (Left(e1), Left(e2))            => Left(e1 ++ e2)
}
```
---
# Version 4 Simplify Syntax

```tut:silent
def validateUserVersion4(user: UserDTO): Either[String, User] = for {
  email <- Email(user.email).toRight(emailError)
  password <- Password(user.password).toRight(passwordError)
} yield User(email, password)
```
---
# Version 5 New Syntax

```tut:silent
import cats.implicits._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

def validateUserVersion5(user: UserDTO): Validated[String, User] = (
  Email(user.email).toValid(emailError),
  Password(user.password).toValid(passwordError)
).mapN(User(_, _))
```
---
# Version 6 Modeling Dependent Errors

```tut:silent
import cats.data.NonEmptyList

sealed trait UserError
final case object PasswordValidationError extends UserError

sealed trait EmailError extends UserError
final case object InvalidEmailError extends EmailError
final case object BlackListedUserError extends EmailError

val blackListedUsers = Seq("bart@simsom.com")

def validateEmailAndEvilness(email: Email)
    : Validated[NonEmptyList[UserError], Email] =
  Validated.condNel(!blackListedUsers.contains(email.value), 
                    email, 
                    BlackListedUserError)

def validateUserVersion6(user: UserDTO): 
   Validated[NonEmptyList[UserError], User] = (
    Email(user.email)
      .toValid(NonEmptyList.of(InvalidEmailError))
      .andThen(validateEmailAndEvilness),
    Password(user.password)
      .toValid(NonEmptyList.of(PasswordValidationError))
  ).mapN(User(_, _))
```
---

# Version 7 Generalizing

Lifting the error to **F[_]**

<img src="images/lift.gif" style="display: block; margin-left: auto;margin-right: auto; width: 75%;"/>

---

# Version 7 Generalizing

```tut:silent
import cats.ApplicativeError

def validateUserVersion7[F[_], E](
        implicit ev: ApplicativeError[F, E],
        evTransform: NonEmptyList[UserError] => E): UserDTO => F[User] = user =>
  ev.fromValidated((
    Email(user.email)
      .toValid(NonEmptyList.of(InvalidEmailError))
      .andThen(validateEmailAndEvilness),
    Password(user.password)
      .toValid(NonEmptyList.of(PasswordValidationError))
  ).mapN(User(_, _)).leftMap(evTransform))

implicit def stringToOptError(error: NonEmptyList[UserError]): Unit = ()
val validateUserVersion1 = validateUserVersion7[Option, Unit]

val validateUserVersion3 = validateUserVersion7[
                            Validated[NonEmptyList[UserError], ?], 
                            NonEmptyList[UserError]]
val validateUserVersion6 = validateUserVersion7[
                            Either[NonEmptyList[UserError], ?], 
                            NonEmptyList[UserError]]
```

---

# Questions?

<img src="images/questions.gif" style="display: block; margin-left: auto;margin-right: auto; width: 75%;"/>