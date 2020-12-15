name := "Quantexa Exercise"
version := "1.0"
scalaVersion := "2.12.12"

mainClass in(Compile, run) := Some("Main")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.apache.spark" %% "spark-sql" % "3.0.1"
libraryDependencies += "org.apache.spark" %% "spark-core" % "3.0.1"
