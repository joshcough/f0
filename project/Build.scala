import java.io.File
import sbt._
import Keys._

object F0Build extends Build {

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
}
