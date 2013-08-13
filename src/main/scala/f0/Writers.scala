package f0

import java.nio.charset.Charset

import Effects.effectW
import Formats._

abstract class Writer[-A,+F] { self =>
  def bind(out: Sink): A => EffectW[F]
  def format[F2>:F](implicit f: F2): F2 = f
  def mapF[F2](implicit f: F => F2): Writer[A,F2] =
    this.asInstanceOf[Writer[A,F2]]
  def toByteArray(a: A): Array[Byte] = {
    val os = new java.io.ByteArrayOutputStream
    Sinks.toOutputStream(os).using(sink => this.bind(sink)(a))
    os.toByteArray
  }
  def apply(a: A)(implicit sink: Sink) = bind(sink)(a)
  def cmap[A0](f: A0 => A): Writer[A0,F] = new Writer[A0,F] {
    def bind(out: Sink) = self.bind(out) compose f
  }
  /** Erases the format type */
  def erase: Writer[A,Nothing] = this.asInstanceOf[Writer[A,Nothing]]
  def |[A0<:A,F0>:F,B,F2,R](w: Writer[B,F2]) = Writers.s2W[A0,F0,B,F2,R](self, w) _
  def selfDescribing[F2>:F](implicit f: F2): Writer[A,P2[StringF,F2]] = Writers.selfDescribing[A,F2](self)
  def orError: Writer[Either[Throwable,A],S2[StringF,F]] = Writers.orError(this)
}

object Writer {
  implicit def toWithLast[A,F](w: Writer[A,F]) = new {
    def withLast[A2,F2](f: A => Writer[A2,F2]) = Writers.withLastW(w)(f)
  }
  def apply[A,F](f: Sink => A => EffectW[F]): Writer[A,F] = new Writer[A,F] {
    def bind(out: Sink) = f(out)
  }
}

object Writers extends SumWriters {

  /** Prepends a string describing this writer's format to the output.
    * This is checked by `Readers.selfDescribing`.
    */
  def selfDescribing[A,F](w: Writer[A,F])(implicit F: F): Writer[A,P2[StringF,F]] =
    p2W(stringW, w)(cb => (a: A) => cb(w.format.asInstanceOf[Format].translate(Languages.canonical), a))

  def withLastW[A,A2,F,F2](w: Writer[A,F])(f: A => Writer[A2,F2]): Writer[(A,A2),P2[F,F2]] = new Writer[(A,A2),F::F2] {
    def bind(o: Sink): ((A,A2)) => EffectW[F::F2] = {
      val bindA = w.bind(o)
      (p: (A,A2)) => { bindA(p._1); f(p._1).bind(o)(p._2).asInstanceOf[EffectW[F::F2]] }
    }
  }

  def p2W[A,F1,B,F2,R](wa: Writer[A,F1], wb: Writer[B,F2])(f: ((A,B) => EffectW[F1::F2]) => R => EffectW[F1::F2]) = new Writer[R,F1::F2] {
    def bind(o: Sink): R => EffectW[F1::F2] = {
      val bindA = wa.bind(o)
      val bindB = wb.bind(o)
      val bindAB = (a: A, b: B) => Write.p2(bindA(a), bindB(b))
      f(bindAB)
    }
  }
  def p3W[A,F1,B,F2,C,F3,R](wa: Writer[A,F1], wb: Writer[B,F2], wc: Writer[C,F3])
                           (f: ((A,B,C) => EffectW[F1::F2::F3]) => R => EffectW[F1::F2::F3]) = new Writer[R,F1::F2::F3] {
    def bind(o: Sink): R => EffectW[F1::F2::F3] = {
      val bindA = wa.bind(o)
      val bindB = wb.bind(o)
      val bindC = wc.bind(o)
      val bindABC = (a: A, b: B, c: C) => Write.p3(bindA(a), bindB(b), bindC(c))
      f(bindABC)
    }
  }
  def p4W[A,F1,B,F2,C,F3,D,F4,R](wa: Writer[A,F1], wb: Writer[B,F2], wc: Writer[C,F3], wd: Writer[D,F4])
                                (f: ((A,B,C,D) => EffectW[F1::F2::F3::F4]) => R => EffectW[F1::F2::F3::F4]) = new Writer[R,F1::F2::F3::F4] {
    def bind(o: Sink): R => EffectW[F1::F2::F3::F4]= {
      val bindA = wa.bind(o)
      val bindB = wb.bind(o)
      val bindC = wc.bind(o)
      val bindD = wd.bind(o)
      val bindABCD = (a: A, b: B, c: C, d: D) => Write.p4(bindA(a), bindB(b), bindC(c), bindD(d))
      f(bindABCD)
    }
  }
  def p5W[A,F1,B,F2,C,F3,D,F4,E,F5,R](wa: Writer[A,F1], wb: Writer[B,F2], wc: Writer[C,F3], wd: Writer[D,F4], we: Writer[E,F5])
                                     (f: ((A,B,C,D,E) => EffectW[F1::F2::F3::F4::F5]) => R => EffectW[F1::F2::F3::F4::F5]) = new Writer[R,F1::F2::F3::F4::F5] {
    def bind(o: Sink): R => EffectW[F1::F2::F3::F4::F5] = {
      val bindA = wa.bind(o)
      val bindB = wb.bind(o)
      val bindC = wc.bind(o)
      val bindD = wd.bind(o)
      val bindE = we.bind(o)
      val bindABCDE = (a: A, b: B, c: C, d: D, e: E) => Write.p5(bindA(a), bindB(b), bindC(c), bindD(d), bindE(e))
      f(bindABCDE)
    }
  }
  def p6W[A,F1,B,F2,C,F3,D,F4,E,F5,F,F6,R](wa: Writer[A,F1], wb: Writer[B,F2], wc: Writer[C,F3], wd: Writer[D,F4], we: Writer[E,F5], wf: Writer[F,F6])
                                     (fun: ((A,B,C,D,E,F) => EffectW[F1::F2::F3::F4::F5::F6]) => R => EffectW[F1::F2::F3::F4::F5::F6]) = new Writer[R,F1::F2::F3::F4::F5::F6] {
    def bind(o: Sink): R => EffectW[F1::F2::F3::F4::F5::F6] = {
      val bindA = wa.bind(o)
      val bindB = wb.bind(o)
      val bindC = wc.bind(o)
      val bindD = wd.bind(o)
      val bindE = we.bind(o)
      val bindF = wf.bind(o)
      val bindABCDEF = (a: A, b: B, c: C, d: D, e: E, f: F) => Write.p6(bindA(a), bindB(b), bindC(c), bindD(d), bindE(e), bindF(f))
      fun(bindABCDEF)
    }
  }

  def tuple2W[A,F1,B,F2](wa: Writer[A,F1], wb: Writer[B,F2]) =
    p2W(wa,wb)(f => (p:(A,B)) => f(p._1,p._2))
  def tuple3W[A,F1,B,F2,C,F3](wa: Writer[A,F1], wb: Writer[B,F2], wc: Writer[C,F3]) =
    p3W(wa,wb,wc)(f => (p:(A,B,C)) => f(p._1,p._2,p._3))
  def tuple4W[A,F1,B,F2,C,F3,D,F4](wa: Writer[A,F1], wb: Writer[B,F2], wc: Writer[C,F3], wd: Writer[D,F4]) =
    p4W(wa,wb,wc,wd)(f => (p:(A,B,C,D)) => f(p._1,p._2,p._3,p._4))
  def tuple5W[A,F1,B,F2,C,F3,D,F4,E,F5](wa: Writer[A,F1], wb: Writer[B,F2], wc: Writer[C,F3], wd: Writer[D,F4], we:Writer[E,F5]) =
    p5W(wa,wb,wc,wd,we)(f => (p:(A,B,C,D,E)) => f(p._1,p._2,p._3,p._4,p._5))
  def tuple6W[A,F1,B,F2,C,F3,D,F4,E,F5,F,F6](wa: Writer[A,F1], wb: Writer[B,F2], wc: Writer[C,F3], wd: Writer[D,F4], we:Writer[E,F5], wf: Writer[F,F6]) =
    p6W(wa,wb,wc,wd,we,wf)(f => (p:(A,B,C,D,E,F)) => f(p._1,p._2,p._3,p._4,p._5,p._6))

  def eitherW[A,F1,B,F2](wa: Writer[A,F1], wb: Writer[B,F2]): Writer[Either[A,B],S2[F1,F2]] =
    s2W(wa,wb)((a,b) => _.fold(a,b))

  def orError[A,F](w: Writer[A,F]): Writer[Either[Throwable,A],S2[StringF,F]] =
    eitherW(
      stringW.cmap((t: Throwable) =>
        t.getMessage + "\n" + t.getStackTrace.mkString("\n")),
      w)

  def optionW[A,F1](w: Writer[A,F1]): Writer[Option[A],S2[UnitF,F1]] =
    s2W(unitW, w)((none, some) => _ match {
      case None => none(())
      case Some(a) => some(a)
    })
  def repeatW[A,F1](w: Writer[A,F1]): Writer[Traversable[A],RepeatF[F1]] = new Writer[Traversable[A],RepeatF[F1]] {
    def bind(o: Sink) = {
      val bindA = w.bind(o)
      (as) => {
        Write.intF(as.size, o)
        as.foreach(bindA)
        effectW[RepeatF[F1]]
      }
    }
  }
  def streamW[A,F1](w: Writer[A,F1]): Writer[TraversableOnce[A],StreamF[F1]] = new Writer[TraversableOnce[A],StreamF[F1]] {
    def bind(o: Sink) = {
      val bindA = w.bind(o)
      (as) => {
        as.foreach(a => { o(1); bindA(a) })
        o(0)
        effectW[StreamF[F1]]
      }
    }
  }

  def unfoldW[S,A,F1](w: Writer[A,F1])(f: S => Option[(A,S)]): Writer[S,StreamF[F1]] = new Writer[S,StreamF[F1]] {
    def bind(o: Sink): S => EffectW[StreamF[F1]] = {
      val bindA = w.bind(o)
      s => {
        var cur = s
        var cont = true
        while (cont) {
          f(cur) match {
            case Some((h,t)) => { o(1); bindA(h); cur = t }
            case None => { cont = false }
          }
        }
        o(0)
        effectW[StreamF[F1]]
      }
    }
  }

  // implement flatMap, one which leaves format type unchanged, and another which is totally dynamic
  implicit def streamF[A](implicit a: A): StreamF[A] = fixF(s2F(p2F(a,selfF), unitF))

  def fixFW[A,F[_]](f: Writer[A,FixF[F[SelfF]]] => Writer[A,F[FixF[F[SelfF]]]]): Writer[A,FixF[F[SelfF]]] =
    fixW(f andThen (_.erase)).asInstanceOf[Writer[A,FixF[F[SelfF]]]]

  def fixW[A,F](f: Writer[A,F] => Writer[A,F]): Writer[A,F] = new Writer[A,F] { self =>
    lazy val fixed = f(self)
    def bind(s: Sink) = {
      lazy val bound = fixed.bind(s)
      a => bound(a)
    }
  }

  def unitW = new Writer[Unit,UnitF] { def bind(o: Sink) = u => effectW[UnitF] }
  def nothingW = new Writer[Nothing,NothingF] { def bind(o: Sink) = n => effectW[NothingF] }
  def booleanW = new Writer[Boolean,BooleanF] { def bind(o: Sink) = Write.booleanF(_, o) }
  def shortW = new Writer[Short,ShortF] { def bind(o: Sink) = Write.shortF(_,o) }
  def intW = new Writer[Int,IntF] { def bind(o: Sink) = Write.intF(_,o) }
  def longW = new Writer[Long,LongF] { def bind(o: Sink) = Write.longF(_,o) }
  def byteW = new Writer[Byte,ByteF] { def bind(o: Sink) = Write.byteF(_,o) }
  def stringW = new Writer[String,StringF] { def bind(o: Sink) = Write.stringF(_, o) }
  def doubleW = new Writer[Double,DoubleF] { def bind(o: Sink) = Write.doubleF(_,o) }
  def floatW = new Writer[Float,FloatF] { def bind(o: Sink) = Write.floatF(_,o) }
}

object Write {
  val utf8 = Charset.forName("UTF-8")
  def stringF(s: String, o: Sink): EffectW[StringF] = {
    val buf = s.getBytes(utf8)
    var i = 0
    intF(buf.length, o)
    while (i < buf.length) { o(buf(i)); i += 1 }
    effectW[StringF]
  }
  def shortF(s: Short, o: Sink): EffectW[ShortF] = {
    o((s >>>  8).asInstanceOf[Byte])
    o((s >>>  0).asInstanceOf[Byte])
    effectW[ShortF]
  }
  def intF(i: Int, o: Sink): EffectW[IntF] = {
    o((i >>> 24).asInstanceOf[Byte])
    o((i >>> 16).asInstanceOf[Byte])
    o((i >>>  8).asInstanceOf[Byte])
    o((i >>>  0).asInstanceOf[Byte])
    effectW[IntF]
  }
  def longF(i: Long, o: Sink): EffectW[LongF] = {
    o((i >>> 56).asInstanceOf[Byte])
    o((i >>> 48).asInstanceOf[Byte])
    o((i >>> 40).asInstanceOf[Byte])
    o((i >>> 32).asInstanceOf[Byte])
    o((i >>> 24).asInstanceOf[Byte])
    o((i >>> 16).asInstanceOf[Byte])
    o((i >>> 8).asInstanceOf[Byte])
    o((i >>> 0).asInstanceOf[Byte])
    effectW[LongF]
  }
  def doubleF(d: Double, o: Sink): EffectW[DoubleF] =
    Write.toDouble(Write.longF(java.lang.Double.doubleToRawLongBits(d), o))
  def floatF(d: Float, o: Sink): EffectW[FloatF] =
    Write.toFloat(Write.intF(java.lang.Float.floatToRawIntBits(d), o))
  def booleanF(b: Boolean, o: Sink): EffectW[BooleanF] = {
    o.writeBit(b); effectW[BooleanF]
  }
  def byteF(b: Byte, o: Sink): EffectW[ByteF] = {
    o(b); effectW[ByteF]
  }
  def p2[F1,F2](f: EffectW[F1], f2: EffectW[F2]): EffectW[F1::F2] = effectW[F1::F2]
  def p3[F1,F2,F3](f: EffectW[F1], f2: EffectW[F2], f3: EffectW[F3]): EffectW[F1::F2::F3] = effectW[F1::F2::F3]
  def p4[F1,F2,F3,F4](f: EffectW[F1], f2: EffectW[F2], f3: EffectW[F3], f4: EffectW[F4]): EffectW[F1::F2::F3::F4] = effectW[F1::F2::F3::F4]
  def p5[F1,F2,F3,F4,F5](f: EffectW[F1], f2: EffectW[F2], f3: EffectW[F3], f4: EffectW[F4], f5: EffectW[F5]): EffectW[F1::F2::F3::F4::F5] = effectW[F1::F2::F3::F4::F5]
  def p6[F1,F2,F3,F4,F5,F6](f: EffectW[F1], f2: EffectW[F2], f3: EffectW[F3], f4: EffectW[F4], f5: EffectW[F5], f6: EffectW[F6]): EffectW[F1::F2::F3::F4::F5::F6] = effectW[F1::F2::F3::F4::F5::F6]

  def toFloat(e: EffectW[IntF]): EffectW[FloatF] = effectW[FloatF]
  def toDouble(e: EffectW[LongF]): EffectW[DoubleF] = effectW[DoubleF]
}
