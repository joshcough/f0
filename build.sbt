name := "f0"

organization := "clarifi"

description := "Streaming I/O for Scala"

version := "1.0"

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

scalaVersion := "2.10.2"

crossScalaVersions := Seq("2.9.2", "2.10.1", "2.10.2")

scalacOptions ++= Seq("-deprecation", "-unchecked")

scalacOptions <++= scalaVersion map {
  case sv if sv.contains("2.10") =>
    Seq("-feature", "-language:implicitConversions", "-language:higherKinds", "-language:existentials", "-language:postfixOps")
  case _ =>
    Seq("-Ydependent-method-types")
}

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"

seq(bintraySettings:_*)

licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

publishMavenStyle := true
