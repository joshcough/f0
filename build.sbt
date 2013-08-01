name := "f0"

organization := "com.clarifi"

description := "Multi-language serialization protocol."

version := "1.0.1"

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

licenses <++= (version)(v => Seq("MIT" -> url("https://github.com/joshcough/f0/blob/%s/LICENSE".format(v))))

publishMavenStyle := true
