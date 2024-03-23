name := "a4-concurrency-fib"
version := "0.1.0"

scalaVersion := "2.13.12"

libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % "2.7.0"

libraryDependencies += "ch.qos.logback"%"logback-classic"%"1.2.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
