name := "Assignment2-2"

version := "1.0"

scalaVersion := "2.13.13"


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

testOptions in Test += Tests.Argument(
  TestFrameworks.ScalaTest, 
  "-u", "tests/test-reports")
