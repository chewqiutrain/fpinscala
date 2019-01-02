name := "fpinscala"

version := "0.1"

scalaVersion := "2.12.4"

//tests
lazy val scalacheck = "org.scalacheck" %% "scalacheck" % "1.13.4"
libraryDependencies += scalacheck % Test

//fs2
// available for Scala 2.11, 2.12
libraryDependencies += "co.fs2" %% "fs2-core" % "0.10.4" // For cats 1.1.0 and cats-effect 0.10
// optional I/O library
libraryDependencies += "co.fs2" %% "fs2-io" % "0.10.4"

// cats
scalacOptions += "-Ypartial-unification"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"


//ostermiller
libraryDependencies += "org.ostermiller" % "utils" % "1.07.00"
libraryDependencies += "com.github.tototoshi" %% "scala-csv" % "1.3.4"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.13",
  "com.typesafe.akka" %% "akka-stream" % "2.5.13",
  "com.typesafe.akka" %% "akka-http" % "10.1.3",
)

