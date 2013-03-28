package f0

object F0 {
  def convert[A,B,F](w: Writer[A,F], r: Reader[B,F]): (A => B) = (a: A) => r(w.toByteArray(a))
}

case class Reg(get: Map[String,Any])

trait Binary[+F] { def source: Source }
// can map over Binary[F] to change its format!
