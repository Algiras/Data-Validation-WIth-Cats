scalaVersion := "2.12.8"

name := "data-validation-with-cats"
organization := "com.wix"
version := "1.0"

resolvers += Resolver.sonatypeRepo("releases")

val catsVersion = "1.6.0"
val specVersion = "4.3.4"
val catsEffectVersion = "1.3.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-effect" % catsEffectVersion,
  "org.specs2" %% "specs2-core" % specVersion % "test"
)


enablePlugins(TutPlugin)

scalacOptions in Test ++= Seq("-Yrangepos", "-Ypartial-unification", "-language:higherKinds")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0")


