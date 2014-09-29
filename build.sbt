name := "rmcode"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

resolvers += Resolver.sonatypeRepo("public")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.1.0" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-core" % "1.4.1"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-canvas" % "1.4.1"

libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-filters" % "1.4.1"
