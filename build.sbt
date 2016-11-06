organization := "edu.cmu.gardner"

name := "semantic_parsing"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

javacOptions += "-Xlint:unchecked"

fork in run := true

libraryDependencies ++= Seq(
  "edu.cmu.ml.rtw" %% "matt-util" % "2.3.2",
  "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
)

instrumentSettings
