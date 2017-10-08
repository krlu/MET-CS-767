import sbt.Keys._

name := "MET_CS_767"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "com.zenecture" %% "neuroflow-core" % "0.900",
  "com.zenecture" %% "neuroflow-application" % "0.900",
  "org.scalatest" % "scalatest_2.12" % "3.2.0-SNAP9" % "test"
)

resolvers ++= Seq("Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/")
