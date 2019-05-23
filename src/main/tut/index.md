
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
# Version 0

Naive implementation that just throws, when validation fails

```tut:silent
class UserValidationException extends Exception("User validation exception")

def validateUserLv0(user: UserDTO): UserDTO = {
  if (isValidEmail(user.email) && isValidPassword(user.password)) {
    user
  } else {
    throw new UserValidationException
  }
}
```

---
# Version 0

<img src="images/boom.gif" style="display: block; margin-left: auto;margin-right: auto; width: 75%;"/>

---
# Version 1

We would like to express the possible failure with the type

```tut:silent
case class Email(value: String)
case class Password(value: String)
case class User(email: Email, password: Password)

def fromUserDTO(user: UserDTO): User = {
  User(Email(user.email), Password(user.password))
}
  
def validateUserLv1(user: UserDTO): Option[User] = {
  if (isValidEmail(user.email) && isValidPassword(user.password)) {
    Some(fromUserDTO(user))
  } else {
    None
  }
}
```

---
#  Version 2

It seems like a reasonable request to express what failed

```tut:silent
val userError = "User validation error"
def validateUserLv2(user: UserDTO): Either[String, User] = {
  if (isValidEmail(user.email) && isValidPassword(user.password)) {
    Right(fromUserDTO(user))
  } else {
    Left(userError)
  }
}

```
---
# Version 3

Providing a mechanism to express what exactly fails provide more flexibility for the user to respond accordingly

```tut:silent
val emailError = "invalid email"
val passwordError = "invalid password"

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
   checkUserField(_.password, isValidPassword, Password, passwordError)(user)
  ) match {
    case (Right(email), Right(password)) => Right(User(email, password))
    case (Left(error), Right(_))         => Left(error)
    case (Right(_), Left(error))         => Left(error)
    case (Left(e1), Left(e2))            => Left(e1 ++ e2)
  }
}
```
---
# Version 4

Introducing simpler syntax

```tut:silent
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
   password <- checkUserField(_.password, isValidPassword, Password, 
                                passwordError)(user)
  } yield User(email, password)
}
```
---
# Version 5

Aggregating errors + nice syntax

```tut:silent
import cats.implicits._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

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
```
---

# Version 6 

Combining aggregating and dependent errors

```tut:silent
import cats.data.NonEmptyList

val evilUserError = "provided email is evil"
val evilEmails = Seq("bart@simsom.com")

def validateEmail(validEmail: Validated[NonEmptyList[String], Email]) = {
  validEmail.andThen(email =>
    if (evilEmails.contains(email.value)) {
      Invalid(NonEmptyList(evilUserError, List.empty))
    } else {
      Valid(email)
    })
}
```
---

# Version 6

Using it with User validation

```tut:silent
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

  val validEmail = validateEmail(
    checkUserField(_.email, isValidEmail, Email, 
                   NonEmptyList(emailError, List.empty))(user)
  )

  (validEmail,
   checkUserField(_.password, isValidPassword, Password, 
                  NonEmptyList(passwordError, List.empty))(user))
    .mapN((email, password) => User(email, password))
}
```
---

# Version 7

Lifting the error to **F[_]**

<img src="images/lift.gif" style="display: block; margin-left: auto;margin-right: auto; width: 75%;"/>

---

# Version 7

```tut:silent
import cats.ApplicativeError
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

  (checkUserField(_.email, isValidEmail, 
                  Email, EmailValidationError)(user),
   checkUserField(_.password, isValidPassword, 
                  Password, PasswordValidationError)(user)
  ).mapN((email: Email, password: Password) => User(email, password))
}
```

---

# Questions?

<img src="images/questions.gif" style="display: block; margin-left: auto;margin-right: auto; width: 75%;"/>