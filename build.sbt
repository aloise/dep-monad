name := "dep-monad"
resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("io.tryp" % "splain" % "0.5.7" cross CrossVersion.patch)

version := "0.3"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.typelevel" %% "cats-core" % "2.1.1"
)

lazy val macros = (project in file("modules/macros"))

lazy val root = (project in file(".")).aggregate(macros)