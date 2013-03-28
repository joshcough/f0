name := "f0"

organization := "clarifi"

version := "v2"

initialCommands := ""

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

//logLevel := Level.Debug

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-Yrecursion", "50", "-deprecation")

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"