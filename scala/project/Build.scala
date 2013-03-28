import java.io.File
import sbt._
import Keys._

object F0Build extends Build {
  val computeRevision = TaskKey[Unit]("compute-revision")
  val ensureNoUncommitted = TaskKey[Unit]("ensure-no-uncommitted")

  val f0settings = Defaults.defaultSettings ++ Seq(
    organization := "clarifi",
    name         := "f0"
  )

  val genSums = TaskKey[Seq[File]]("gen-sums", "Generates code for Sum writers.")

  lazy val project = Project (
    "f0",
    file ("."),
    settings  = f0settings ++ Seq(
      genSums <<= (sourceDirectory, streams) map { (dir,s) =>
        s.log.info("generating sum code to: " + dir)
        CodegenScala(dir) ++ CodegenFSharp(dir)
      }
    )
  )

//    lazy val project = Project (
//    "Reporting",
//    file ("."),
//    settings = Defaults.defaultSettings ++ Set(fullRunInputTask(repl, Compile, "com.clarifi.reporting.dmtl.DMTLParser"))
//  )
  /**
   *  val f0settings = Defaults.defaultSettings ++ Seq(
    organization := "clarifi",
    name         := "f0",
    version      := "0.2",
    scalaVersion := "2.9.0-1",
    libraryDependencies += "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9",
    externalIvySettings(baseDirectory(_ => file("""\\ciqbos-filesvr1\BostonSoftwareDepot\ivy\ivysettings.xml"""))),
    externalIvyFile( baseDirectory { base => base / "ivy.xml"} ),
    publishTo := Some(Resolver.file("clarifi-repository", file("""Z://ivy""")))
  )
   */
}
