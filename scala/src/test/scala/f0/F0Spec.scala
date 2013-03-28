package f0

import org.scalacheck._
import Prop._

import f0.Readers._
import f0.Writers._
import f0.Formats._

object F0Spec extends Properties("F0") {

  implicit val stringArb = Arbitrary(Gen.alphaStr)
  def clone[A:Arbitrary,F](w: Writer[A,F], r: Reader[A,F]): Prop =
    forAll((a: A) => {
      (r(w.toByteArray(a)) ?= a)
    })

  property("int") = clone(intW, intR)
  property("long") = clone(longW, longR)
  property("double") = clone(doubleW, doubleR)
  property("float") = clone(floatW, floatR)
  property("byte") = clone(byteW, byteR)
  property("longs") = clone(repeatW(longW), listR(longR))
  property("booleans") = clone(repeatW(booleanW), listR(booleanR))
  property("nested") = clone(repeatW(repeatW(doubleW)), listR(listR(doubleR)))
  property("string") = clone(stringW, stringR)
  property("stream") = clone(streamW(stringW), streamR(stringR))
  property("either") = clone(eitherW(intW,stringW), eitherR(intR,stringR))
  property("eithers") = clone(repeatW(eitherW(intW,stringW)), listR(eitherR(intR,stringR)))
  property("stream of streams") = clone(streamW(streamW(stringW)), streamR(streamR(stringR)))

  property("-4.712236580071058") = {
    val d = -4.709036388694336
    doubleR(doubleW.toByteArray(d)) == d
  }

  implicit val arbS5 = Arbitrary { for {
    i <- Arbitrary.arbitrary[Int]
    t <- Gen.choose(1,1000)
  } yield {
    val x = t % 5
    x match {
      case 0 => Uno(i)
      case 1 => Dos(i)
      case 2 => Tres(i)
      case 3 => Quatro(i)
      case 4 => Cinco(i)
    }
  }}

  trait S5Int { def get: Int }
  case class Uno(get: Int) extends S5Int
  case class Dos(get: Int) extends S5Int
  case class Tres(get: Int) extends S5Int
  case class Quatro(get: Int) extends S5Int
  case class Cinco(get: Int) extends S5Int

  property("sum5") = clone(
    s5W(intW,intW,intW,intW,intW)((a,b,c,d,e) => (i: S5Int) => i match {
      case Uno(x) => a(x)
      case Dos(x) => b(x)
      case Tres(x) => c(x)
      case Quatro(x) => d(x)
      case Cinco(x) => e(x)
    }), s5R(intR, intR, intR, intR, intR)(Uno, Dos,  Tres, Quatro, Cinco)
  )

  property("withLastW") = secure {
    val w = intW withLast (i => intW cmap ((j: Int) => j + i))
    val out = w.toByteArray((1,3))
    val (x,y) = tuple2R(intR, intR)(out)
    y == 4
  }

  property("nestedStream") = clone(streamW(repeatW(tuple2W(stringW,intW))), streamR(listR(tuple2R(stringR,intR))))

  property("selfDescribing") = clone(
    streamW(repeatW(tuple2W(stringW,intW))).selfDescribing, 
    streamR(listR(tuple2R(stringR,intR))).selfDescribing)
}
