package f0

import Formats._

trait Get[+A] { def get: A }

trait Reader[+A,-F] {
  def bind(s: Source): Get[A]
  def apply(s: Source) = bind(s).get
  def apply(bytes: Array[Byte]): A =
    bind(Sources.fromArray(bytes)).get
  def map[B](f: A => B): Reader[B,F] = new Reader[B,F] {
    def bind(s: Source): Get[B] = new Get[B] {
      val outer = Reader.this.bind(s)
      def get = f(outer.get)
    }
  }
  def flatMap[B,F2](f: A => Reader[B,F2]): Reader[B,P2[F,F2]] = new Reader[B,P2[F,F2]] {
    def bind(s: Source): Get[B] = new Get[B] {
      val outer = Reader.this.bind(s)
      def get = f(outer.get).bind(s).get
    }
  }
  /** Erases the format type for this Reader. */
  def erase: Reader[A,Any] =
    this.asInstanceOf[Reader[A,Any]]
  def cmapF[F0](implicit f: F0 => F): Reader[A,F0] =
    this.asInstanceOf[Reader[A,F0]]
  def union[B>:A,F2](w: Reader[B,F2]): Reader[B,S2[F,F2]] =
    Readers.union2R(this, w)
  def |[B>:A,F2](w: Reader[B,F2]): Reader[B,S2[F,F2]] =
    union(w)

  def selfDescribing[B>:A](implicit F: F): Reader[B,P2[StringF,F]] =
    Readers.selfDescribing[B,F](this)(F)

  def label(msg: String): Reader[A,F] = new Reader[A,F] {
    def bind(s: Source): Get[A] = new Get[A] {
      val outer = Reader.this.bind(s)
      val singleton = List(msg)
      def get =
        try outer.get
        catch {
          case ReadError(msgs, cause) => throw ReadError(msg :: msgs, cause)
          case e: Exception => throw ReadError(singleton, e)
        }
    }
  }
}

case class ReadError(msgs: List[String], cause: Throwable) extends RuntimeException(cause) {
  override def getMessage =
    "markers:\n" + msgs.mkString("\n") +
    "\ncause: " + cause.getMessage
}

object Readers extends SumReaders {
  val unitR: Reader[Unit, UnitF] = new Reader[Unit, UnitF] {
    def bind(s: Source): Get[Unit] = new Get[Unit] {
      def get = ()
    }
  }

  val nothingR: Reader[Nothing, NothingF] = new Reader[Nothing, NothingF] {
    def bind(s: Source): Get[Nothing] = new Get[Nothing] {
      def get = sys.error("Nothing")
    }
  }

  def selfDescribing[A,F](r: Reader[A,F])(implicit F: F): Reader[A,P2[StringF,F]] =
    stringR.flatMap { fmt =>
      val fmtExpected = F.asInstanceOf[Format].translate(Languages.canonical)
      if (fmtExpected != fmt)
        sys.error("format mismatch:\nreader expected:\n%s\nbut got:\n%s".format(fmtExpected, fmt))
      r
    }

  def p2R[A,F1,B,F2,R](ra: Reader[A,F1], rb: Reader[B,F2])
                      (f: (A,B) => R) = new Reader[R,F1::F2] {
    def bind(s: Source): Get[R] = new Get[R] {
      val a = ra.bind(s)
      val b = rb.bind(s)
      def get = f(a.get, b.get)
    }
  }
  def p3R[A,F1,B,F2,C,F3,R](ra: Reader[A,F1], rb: Reader[B,F2], rc: Reader[C,F3])
                           (f: (A,B,C) => R) = new Reader[R,F1::F2::F3] {
    def bind(s: Source): Get[R] = new Get[R] {
      val a = ra.bind(s)
      val b = rb.bind(s)
      val c = rc.bind(s)
      def get = f(a.get, b.get, c.get)
    }
  }
  def p4R[A,F1,B,F2,C,F3,D,F4,R](ra: Reader[A,F1], rb: Reader[B,F2], rc: Reader[C,F3], rd: Reader[D,F4])
                                (f: (A,B,C,D) => R) = new Reader[R,F1::F2::F3::F4] {
    def bind(s: Source): Get[R] = new Get[R] {
      val a = ra.bind(s)
      val b = rb.bind(s)
      val c = rc.bind(s)
      val d = rd.bind(s)
      def get = f(a.get, b.get, c.get, d.get)
    }
  }
  def p5R[A,F1,B,F2,C,F3,D,F4,E,F5,R](ra: Reader[A,F1], rb: Reader[B,F2], rc: Reader[C,F3], rd: Reader[D,F4], re: Reader[E,F5])
                                     (f: (A,B,C,D,E) => R) = new Reader[R,F1::F2::F3::F4::F5] {
    def bind(s: Source): Get[R] = new Get[R] {
      val a = ra.bind(s)
      val b = rb.bind(s)
      val c = rc.bind(s)
      val d = rd.bind(s)
      val e = re.bind(s)
      def get = f(a.get, b.get, c.get, d.get, e.get)
    }
  }
  def p6R[A,F1,B,F2,C,F3,D,F4,E,F5,F,F6,R]
    (ra: Reader[A,F1], rb: Reader[B,F2], rc: Reader[C,F3], rd: Reader[D,F4], re: Reader[E,F5], rf: Reader[F,F6])
    (p: (A,B,C,D,E,F) => R): Reader[R,F1::F2::F3::F4::F5::F6] = new Reader[R,F1::F2::F3::F4::F5::F6] {
    def bind(s: Source): Get[R] = new Get[R] {
      val a = ra.bind(s)
      val b = rb.bind(s)
      val c = rc.bind(s)
      val d = rd.bind(s)
      val e = re.bind(s)
      val f = rf.bind(s)
      def get = p(a.get, b.get, c.get, d.get, e.get, f.get)
    }
  }

  def tuple2R[A,F1,B,F2](ra: Reader[A,F1], rb: Reader[B,F2]): Reader[(A,B),F1::F2] =
    p2R(ra,rb)((_,_))
  def tuple3R[A,F1,B,F2,C,F3](ra: Reader[A,F1], rb: Reader[B,F2], rc: Reader[C,F3]): Reader[(A,B,C),F1::F2::F3] =
    p3R(ra,rb,rc)((_,_,_))
  def tuple4R[A,F1,B,F2,C,F3,D,F4](ra: Reader[A,F1], rb: Reader[B,F2], rc: Reader[C,F3], rd: Reader[D,F4]): Reader[(A,B,C,D),F1::F2::F3::F4] =
    p4R(ra,rb,rc,rd)((_,_,_,_))
  def tuple5R[A,F1,B,F2,C,F3,D,F4,E,F5](ra: Reader[A,F1], rb: Reader[B,F2], rc: Reader[C,F3], rd: Reader[D,F4], re: Reader[E,F5]): Reader[(A,B,C,D,E),F1::F2::F3::F4::F5] =
    p5R(ra,rb,rc,rd,re)((_,_,_,_,_))
  def tuple6R[A,F1,B,F2,C,F3,D,F4,E,F5,F,F6](ra: Reader[A,F1], rb: Reader[B,F2], rc: Reader[C,F3], rd: Reader[D,F4], re: Reader[E,F5], rf: Reader[F,F6]): Reader[(A,B,C,D,E,F),F1::F2::F3::F4::F5::F6] =
    p6R(ra,rb,rc,rd,re,rf)((_,_,_,_,_,_))

  def eitherR[A,F1,B,F2](ra: Reader[A,F1], rb: Reader[B,F2]): Reader[Either[A,B],S2[F1,F2]] = s2R(ra,rb)(Left(_), Right(_))
  def optionR[A,F1](ra: Reader[A,F1]): Reader[Option[A],S2[UnitF,F1]] = union2R(unitR map (_ => None), ra map (Some(_)))

  val booleanR: Reader[Boolean,BooleanF] = new Reader[Boolean,BooleanF] {
    def bind(s: Source): Get[Boolean] = new Get[Boolean] { def get = Read.booleanF(s) }
  }
  val byteR: Reader[Byte,ByteF] = new Reader[Byte,ByteF] {
    def bind(s: Source): Get[Byte] = new Get[Byte] { def get = Read.byteF(s) }
  }
  val intR: Reader[Int,IntF] = new Reader[Int,IntF] {
    def bind(s: Source): Get[Int] = new Get[Int] { def get = Read.intF(s) }
  }
  val longR: Reader[Long,LongF] = new Reader[Long,LongF] {
    def bind(s: Source): Get[Long] = new Get[Long] { def get = Read.longF(s) }
  }
  val shortR: Reader[Short, ShortF] = new Reader[Short, ShortF] {
    def bind(s: Source): Get[Short] = new Get[Short] { def get = Read.shortF(s) }
  }
  val doubleR: Reader[Double,DoubleF] = new Reader[Double,DoubleF] {
    def bind(s: Source): Get[Double] = new Get[Double] { def get = Read.doubleF(s) }
  }
  val floatR: Reader[Float,FloatF] = new Reader[Float,FloatF] {
    def bind(s: Source): Get[Float] = new Get[Float] { def get = Read.floatF(s) }
  }
  val stringR: Reader[String,StringF] = new Reader[String,StringF] {
    def bind(s: Source): Get[String] = new Get[String] { def get = Read.stringF(s) }
  }

  def foldR[A,B,F](r: Reader[A,F])(z: B)(f: (B,A) => B): Reader[B,RepeatF[F]] = new Reader[B,RepeatF[F]] {
    def bind(s: Source): Get[B] = new Get[B] {
      val elem = r.bind(s)
      def get = {
        var n = Read.intF(s)
        var acc = z
        while (n > 0) { acc = f(acc, elem.get); n -= 1 }
        acc
      }
    }
  }

  def foldStreamR[A,B,F](r: Reader[A,F])(z: B)(f: (B,A) => B): Reader[B,StreamF[F]] = new Reader[B,StreamF[F]] {
    def bind(s: Source): Get[B] = new Get[B] {
      val elem = lazyStreamR(r).bind(s)
      def get = {
        var acc = z
        var cur = elem.get
        while (cur != None) { acc = f(acc, cur.get); cur = elem.get }
        acc
      }
    }
  }

  def lazyStreamR[A,F](r: Reader[A,F]): Reader[Option[A],StreamF[F]] = new Reader[Option[A],StreamF[F]] {
    def bind(s: Source): Get[Option[A]] = new Get[Option[A]] {
      val elem = r.bind(s)
      var eof = false
      def get = {
        eof = s.readBit
        if (eof) { eof = false; None }
        else Some(elem.get)
      }
    }
  }

  def treeR[A,R,F](r: Reader[A,F])(branch: (A,List[R]) => R): Reader[R,TreeF[F]] = new Reader[R, TreeF[F]] { self =>
    def bind(s: Source): Get[R] = new Get[R] {
      val elem = r.bind(s)
      val forest = listR(self).bind(s)
      def get = branch(elem.get, forest.get)
    }
  }

  def fixFR[A,F[_]](fn: Reader[A,FixF[F[SelfF]]] => Reader[A,F[FixF[F[SelfF]]]]): Reader[A,FixF[F[SelfF]]] =
    fixR(fn andThen (_.erase)).asInstanceOf[Reader[A,FixF[F[SelfF]]]]

  def fixR[A,F](fn: Reader[A,F] => Reader[A,F]): Reader[A,F] = new Reader[A,F] { self =>
    lazy val fixed = fn(self)
    def bind(s: Source): Get[A] = new Get[A] {
      lazy val bound = fixed.bind(s)
      def get = bound.get
    }
  }

  def listR[A,F](r: Reader[A,F]): Reader[List[A],RepeatF[F]] =
    foldR(r)(List[A]())((buf,a) => a :: buf) map (_.reverse)

  def streamR[A,F](r: Reader[A,F]): Reader[List[A],StreamF[F]] =
    foldStreamR(r)(List[A]())((buf,a) => a :: buf) map (_.reverse)

  def attempt[A,F](r: Reader[A,F]) = new Reader[Either[Exception,A],F] {
    def bind(s: Source) = new Get[Either[Exception,A]] {
      val elem = r.bind(s)
      def get = try Right(elem.get) catch { case e:Exception => Left(e) }
    }
  }

  // for lang without tctor poly, just pass a regular function from Format to Reader[A,_],
  // return type is the same - just won't check that function doesn't do something evil statically
  // dependent product
  // Dependent[Fn] means read a format, then substitute that format into body of Fn for Arg
  // def dfold[G[_],A](r: Reader[Format,FormatF])(f: DynamicReader[G,List[A]]): Reader[List[A],Dependent[ApplyF[ListF,Arg]]]
}

//trait DynamicReader[G[_]] { def apply[F<:Format](f: F): Reader[A,G[F]] }

object Read {
  def booleanF(s: Source): Boolean = s.readBit
  def byteF(s: Source): Byte = s.readByte
  def intF(s: Source): Int =
    (s.readByte.asInstanceOf[Int] << 24) |
    ((0xFF & s.readByte.asInstanceOf[Int]) << 16) |
    ((0xFF & s.readByte.asInstanceOf[Int]) << 8)  |
    ((0xFF & s.readByte.asInstanceOf[Int]))
 def shortF(s: Source): Short =
    ((s.readByte.asInstanceOf[Int] << 8) |
    (0xFF & s.readByte.asInstanceOf[Int])).toShort
 def longF(s: Source): Long =
    (s.readByte.asInstanceOf[Long] << 56) |
    ((0xFF & s.readByte.asInstanceOf[Long]) << 48) |
    ((0xFF & s.readByte.asInstanceOf[Long]) << 40) |
    ((0xFF & s.readByte.asInstanceOf[Long]) << 32) |
    ((0xFF & s.readByte.asInstanceOf[Long]) << 24) |
    ((0xFF & s.readByte.asInstanceOf[Long]) << 16) |
    ((0xFF & s.readByte.asInstanceOf[Long]) << 8)  |
    ((0xFF & s.readByte.asInstanceOf[Long]))

  def doubleF(s: Source): Double =
    java.lang.Double.longBitsToDouble(Read.longF(s))
  def floatF(s: Source): Float =
    java.lang.Float.intBitsToFloat(Read.intF(s))

  val utf8 = Write.utf8
  def stringF(s: Source): String = {
    val n    = Read.intF(s)
    val buf  = s.readBytes(n)
    // this didn't work, but we don't really know why.
    // it seems to add some trailing bytes (white space) for non-ascii strings.
    //val cbuf = utf8.decode(java.nio.ByteBuffer.wrap(buf)).array
    new String(buf, utf8)
  }
}
