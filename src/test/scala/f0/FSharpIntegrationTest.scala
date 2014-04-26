package f0
import Readers._
import Writers._
import org.scalacheck.Prop._
import org.scalacheck.Properties

/**
 * This class does some integration testing with F#.
 * Before this test can run, the f# f0 solution must be compiled.
 * That is currently done by hand in Visual Studio.
 *
 * For now, I'm going to check in fsharp/f0/RoundTripTester/bin/Debug/RoundTripTester.exe so that this
 * test will run without having to build via VS.
 */
object FSharpIntegrationTest extends Properties("FSharpIntegrationTest"){

  // these are the values that are written out by the f# program
  val values = List.fill(10000)(
    (
      -4.712236580071058,
      (7, true, false, 42, (9223372036854775807L, -9223372036854775808L, 0L), "hello, world"),
      (3.1415, 3.1415f, List(1,2,3,4,5), (1,2), (1,2,3), List(1,2,3,4,5))
    )
  )
  // and the corresponding reader and writer for those values.
  val reader = listR(
    tuple3R(
      doubleR,
      tuple6R(intR, booleanR, booleanR, intR, tuple3R(longR, longR, longR), stringR),
      tuple6R(doubleR, floatR, listR(intR), tuple2R(intR, intR), tuple3R(intR, intR, intR), streamR(intR)))
  )

  val writer = repeatW(
    tuple3W(
      doubleW,
      tuple6W(intW, booleanW, booleanW, intW, tuple3W(longW, longW, longW), stringW),
      tuple6W(doubleW, floatW, repeatW(intW), tuple2W(intW, intW), tuple3W(intW, intW, intW), streamW(intW)))
  )

  // just to make sure things are sane before doing the actual integration test,
  // write and read the values to a byte array
  property("read and write to byte array") = secure {
    val ms = new java.io.ByteArrayOutputStream()
    val result = writeAndReadAllValues(Sinks.toOutputStream(ms), {
      //println(ms.toByteArray.toList)
      Sources.fromArray(ms.toByteArray)
    })
    //println("result: " + result)
    values ?= result
  }
  // similar to the property before, this is a sanity check
  // make sure that we can write and read to files
  property("read and write to file") = secure {
    // writes to f0/test/data-from-scala.dat
    val filename = "test/data-from-scala.dat"
    val f = new java.io.File(filename); f.delete(); f.createNewFile()
    values ?= writeAndReadAllValues(Sinks.toFile(filename), Sources.fromFile(filename))
  }

  // finally, do the integration testing
  val fSharpRoundTripTester = "fsharp/f0/RoundTripTester/bin/Debug/RoundTripTester.exe"
  property("write to and then read from f# process") =
    new java.io.File(fSharpRoundTripTester).exists ==> secure {
      val p = Runtime.getRuntime.exec(fSharpRoundTripTester)
      val result = writeAndReadAllValues(Sinks.toProcess(p), Sources.fromProcess(p))
      //println(result)
      values ?= result
    }

  // writes all the 'values' to the given sink,
  // then reads them all back in from the given source
  def writeAndReadAllValues(sink:Sink, source: => Source) = {
    writer.bind(sink)(values); sink.close; reader.bind(source).get
  }
}
