name := "dep-monad"

addCompilerPlugin("io.tryp" % "splain" % "0.4.0" cross CrossVersion.patch)

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3",
  "org.typelevel" %% "cats-core" % "1.6.0"
)