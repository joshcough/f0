package f0

import Formats.Implicits.asFormat

trait Format {
  /** Converts the type corresponding to this format to a String representing this type in the target language. */
  def translate(l: Language): String
}

case class UnitF() extends Format { def translate(l: Language) = "UnitF" }
case class NothingF() extends Format { def translate(l: Language) = "NothingF" }
case class ShortF() extends Format { def translate(l: Language) = "ShortF" }
case class IntF() extends Format { def translate(l: Language) = "IntF" }
case class DoubleF() extends Format { def translate(l: Language) = "DoubleF" }
case class FloatF() extends Format { def translate(l: Language) = "FloatF" }
case class StringF() extends Format { def translate(l: Language) = "StringF" }
case class BooleanF() extends Format { def translate(l: Language) = "BooleanF" }
case class LongF() extends Format { def translate(l: Language) = "LongF" }
case class ByteF() extends Format { def translate(l: Language) = "ByteF" }

case class P2[+A,+B](a: A, b: B) extends Format {
  def translate(l: Language) = l.typeApplication("P2", a translate l, b translate l)
}
// type F = (A | B) | (C | D) // problem is - for A = 0, B = 1, C = 2, D = 3, need to write out MSB first
//
// type F = (A | B) | C
//trait S
//case class SNil() extends Format with S {
//  def translate(l: Language) = "SNil"
//}
//case class SCons[+A,+B<:S](h: A, t: B) extends Format with S {
//  def translate(l: Language) =
case class RepeatF[+A](a: A) extends Format { def translate(l: Language) = l.typeApplication("RepeatF", a translate l) }
case class AliasF[+A](a: A) extends Format { def translate(l: Language) = l.typeApplication("AliasF", a translate l) }
case class MemoF[F](f: F) extends Format { def translate(l: Language) = l.typeApplication("MemoF", f translate l) }
case class FixF[F](f: F) extends Format { def translate(l: Language) = l.typeApplication("FixF", f translate l) }
case class SelfF() extends Format { def translate(l: Language) = "SelfF" }
case class ArgF() extends Format { def translate(l: Language) = "ArgF" }
case class ApF[F,Arg](f: F, a: Arg) extends Format { def translate(l: Language) = l.typeApplication("ApF", f translate l, a translate l) }
case class DynamicF() extends Format { def translate(l: Language) = "DynamicF" }

object Formats extends SumFormats {
  type ::[A,B] = P2[A,B]
  type StreamF[A] = FixF[S2[A::SelfF, UnitF]]
  type TreeF[A] = FixF[A::RepeatF[SelfF]]

  object Implicits {
    implicit def asFormat[A](f: A): Format = f.asInstanceOf[Format]
  }

  implicit val unitF: UnitF = UnitF()
  implicit val shortF: ShortF = ShortF()
  implicit val intF: IntF = IntF()
  implicit val doubleF: DoubleF = DoubleF()
  implicit val floatF: FloatF = FloatF()
  implicit val stringF: StringF = StringF()
  implicit val booleanF: BooleanF = BooleanF()
  implicit val longF: LongF = LongF()
  implicit val byteF: ByteF = ByteF()
  implicit val selfF: SelfF = SelfF()
  implicit val argF: ArgF = ArgF()
  implicit val dynamicF: DynamicF = DynamicF()

  implicit def p2F[A,B](implicit a: A, b: B): A::B = P2(a, b)
  implicit def repeatF[A](implicit a: A) = RepeatF(a)
  implicit def fixF[F](implicit f: F) = FixF(f)
  implicit def streamF[A](implicit a: A): StreamF[A] = fixF(s2F(p2F(a,selfF), unitF))

  type |[A,B] = S2[A,B]
  type &[A,B] = P2[A,B]

  //def evidence[F](implicit f: F): F = f
}
