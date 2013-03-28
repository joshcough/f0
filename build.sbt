name := "f0"

organization := "clarifi"

version := "v2"

initialCommands := ""

publishArtifact in (Compile, packageDoc) := false

publishArtifact in (Compile, packageSrc) := false

//logLevel := Level.Debug

scalaVersion := "2.9.2"

crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.0", "2.10.1")

scalacOptions <++= (scalaVersion) map { sv =>
  val versionDepOpts =
    if (sv startsWith "2.9") Seq()
    else Seq("-feature", "-language:higherKinds", "-language:implicitConversions")
  Seq("-Yrecursion", "50", "-deprecation", "-unchecked") ++ versionDepOpts
}

resolvers += "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.0" % "test"