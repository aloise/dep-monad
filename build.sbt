name := "dep-monad"
resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("io.tryp" % "splain" % "0.4.0" cross CrossVersion.patch)

version := "0.2"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.typelevel" %% "cats-core" % "1.6.0"
)



lazy val macros = (project in file("modules/macros"))

lazy val root = (project in file(".")).aggregate(macros)