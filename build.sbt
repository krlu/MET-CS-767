import sbt.Keys._

name := "MET_CS_767"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "com.zenecture" % "neuroflow-core_2.12" % "1.2.5",
  "com.zenecture" % "neuroflow-application_2.12" % "1.2.5",
  "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP9" % "test",
  "com.cra.figaro" % "figaro_2.12" % "5.0.0.0"
)

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/")
