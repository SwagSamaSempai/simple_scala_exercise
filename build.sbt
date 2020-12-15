name := "Quantexa Exercise"
version := "1.0"
scalaVersion := "2.13.4"

mainClass in(Compile, run) := Some("Main")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
