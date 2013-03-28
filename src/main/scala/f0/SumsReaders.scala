package f0

import Formats._

trait SumReaders {
    
  /** 2-way sum reader */
  def s2R[A0,F0,A1,F1,R](r0: Reader[A0,F0], r1: Reader[A1,F1])(f0: A0 => R, f1: A1 => R) = new Reader[R,S2[F0,F1]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union2R[A,F0,F1](a0: Reader[A,F0], a1: Reader[A,F1]): Reader[A,S2[F0,F1]] =
    s2R(a0, a1)(identity, identity)

  /** 3-way sum reader */
  def s3R[A0,F0,A1,F1,A2,F2,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2])(f0: A0 => R, f1: A1 => R, f2: A2 => R) = new Reader[R,S3[F0,F1,F2]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union3R[A,F0,F1,F2](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2]): Reader[A,S3[F0,F1,F2]] =
    s3R(a0, a1, a2)(identity, identity, identity)

  /** 4-way sum reader */
  def s4R[A0,F0,A1,F1,A2,F2,A3,F3,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R) = new Reader[R,S4[F0,F1,F2,F3]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union4R[A,F0,F1,F2,F3](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3]): Reader[A,S4[F0,F1,F2,F3]] =
    s4R(a0, a1, a2, a3)(identity, identity, identity, identity)

  /** 5-way sum reader */
  def s5R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R) = new Reader[R,S5[F0,F1,F2,F3,F4]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union5R[A,F0,F1,F2,F3,F4](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4]): Reader[A,S5[F0,F1,F2,F3,F4]] =
    s5R(a0, a1, a2, a3, a4)(identity, identity, identity, identity, identity)

  /** 6-way sum reader */
  def s6R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R) = new Reader[R,S6[F0,F1,F2,F3,F4,F5]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union6R[A,F0,F1,F2,F3,F4,F5](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5]): Reader[A,S6[F0,F1,F2,F3,F4,F5]] =
    s6R(a0, a1, a2, a3, a4, a5)(identity, identity, identity, identity, identity, identity)

  /** 7-way sum reader */
  def s7R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R) = new Reader[R,S7[F0,F1,F2,F3,F4,F5,F6]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union7R[A,F0,F1,F2,F3,F4,F5,F6](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6]): Reader[A,S7[F0,F1,F2,F3,F4,F5,F6]] =
    s7R(a0, a1, a2, a3, a4, a5, a6)(identity, identity, identity, identity, identity, identity, identity)

  /** 8-way sum reader */
  def s8R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R) = new Reader[R,S8[F0,F1,F2,F3,F4,F5,F6,F7]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union8R[A,F0,F1,F2,F3,F4,F5,F6,F7](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7]): Reader[A,S8[F0,F1,F2,F3,F4,F5,F6,F7]] =
    s8R(a0, a1, a2, a3, a4, a5, a6, a7)(identity, identity, identity, identity, identity, identity, identity, identity)

  /** 9-way sum reader */
  def s9R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R) = new Reader[R,S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union9R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8]): Reader[A,S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]] =
    s9R(a0, a1, a2, a3, a4, a5, a6, a7, a8)(identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 10-way sum reader */
  def s10R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R) = new Reader[R,S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union10R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9]): Reader[A,S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]] =
    s10R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 11-way sum reader */
  def s11R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R) = new Reader[R,S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union11R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10]): Reader[A,S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]] =
    s11R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 12-way sum reader */
  def s12R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R) = new Reader[R,S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union12R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11]): Reader[A,S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]] =
    s12R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 13-way sum reader */
  def s13R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R) = new Reader[R,S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union13R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12]): Reader[A,S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]] =
    s13R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 14-way sum reader */
  def s14R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R) = new Reader[R,S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union14R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13]): Reader[A,S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]] =
    s14R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 15-way sum reader */
  def s15R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13], r14: Reader[A14,F14])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R, f14: A14 => R) = new Reader[R,S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      val bindr14 = r14.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case 14 => f14(bindr14.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union15R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13], a14: Reader[A,F14]): Reader[A,S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]] =
    s15R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 16-way sum reader */
  def s16R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13], r14: Reader[A14,F14], r15: Reader[A15,F15])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R, f14: A14 => R, f15: A15 => R) = new Reader[R,S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      val bindr14 = r14.bind(s)
      val bindr15 = r15.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case 14 => f14(bindr14.get)
        case 15 => f15(bindr15.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union16R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13], a14: Reader[A,F14], a15: Reader[A,F15]): Reader[A,S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]] =
    s16R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 17-way sum reader */
  def s17R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13], r14: Reader[A14,F14], r15: Reader[A15,F15], r16: Reader[A16,F16])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R, f14: A14 => R, f15: A15 => R, f16: A16 => R) = new Reader[R,S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      val bindr14 = r14.bind(s)
      val bindr15 = r15.bind(s)
      val bindr16 = r16.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case 14 => f14(bindr14.get)
        case 15 => f15(bindr15.get)
        case 16 => f16(bindr16.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union17R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13], a14: Reader[A,F14], a15: Reader[A,F15], a16: Reader[A,F16]): Reader[A,S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]] =
    s17R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 18-way sum reader */
  def s18R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13], r14: Reader[A14,F14], r15: Reader[A15,F15], r16: Reader[A16,F16], r17: Reader[A17,F17])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R, f14: A14 => R, f15: A15 => R, f16: A16 => R, f17: A17 => R) = new Reader[R,S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      val bindr14 = r14.bind(s)
      val bindr15 = r15.bind(s)
      val bindr16 = r16.bind(s)
      val bindr17 = r17.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case 14 => f14(bindr14.get)
        case 15 => f15(bindr15.get)
        case 16 => f16(bindr16.get)
        case 17 => f17(bindr17.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union18R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13], a14: Reader[A,F14], a15: Reader[A,F15], a16: Reader[A,F16], a17: Reader[A,F17]): Reader[A,S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]] =
    s18R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 19-way sum reader */
  def s19R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,A18,F18,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13], r14: Reader[A14,F14], r15: Reader[A15,F15], r16: Reader[A16,F16], r17: Reader[A17,F17], r18: Reader[A18,F18])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R, f14: A14 => R, f15: A15 => R, f16: A16 => R, f17: A17 => R, f18: A18 => R) = new Reader[R,S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      val bindr14 = r14.bind(s)
      val bindr15 = r15.bind(s)
      val bindr16 = r16.bind(s)
      val bindr17 = r17.bind(s)
      val bindr18 = r18.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case 14 => f14(bindr14.get)
        case 15 => f15(bindr15.get)
        case 16 => f16(bindr16.get)
        case 17 => f17(bindr17.get)
        case 18 => f18(bindr18.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union19R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13], a14: Reader[A,F14], a15: Reader[A,F15], a16: Reader[A,F16], a17: Reader[A,F17], a18: Reader[A,F18]): Reader[A,S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]] =
    s19R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 20-way sum reader */
  def s20R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,A18,F18,A19,F19,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13], r14: Reader[A14,F14], r15: Reader[A15,F15], r16: Reader[A16,F16], r17: Reader[A17,F17], r18: Reader[A18,F18], r19: Reader[A19,F19])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R, f14: A14 => R, f15: A15 => R, f16: A16 => R, f17: A17 => R, f18: A18 => R, f19: A19 => R) = new Reader[R,S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      val bindr14 = r14.bind(s)
      val bindr15 = r15.bind(s)
      val bindr16 = r16.bind(s)
      val bindr17 = r17.bind(s)
      val bindr18 = r18.bind(s)
      val bindr19 = r19.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case 14 => f14(bindr14.get)
        case 15 => f15(bindr15.get)
        case 16 => f16(bindr16.get)
        case 17 => f17(bindr17.get)
        case 18 => f18(bindr18.get)
        case 19 => f19(bindr19.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union20R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13], a14: Reader[A,F14], a15: Reader[A,F15], a16: Reader[A,F16], a17: Reader[A,F17], a18: Reader[A,F18], a19: Reader[A,F19]): Reader[A,S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]] =
    s20R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 21-way sum reader */
  def s21R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,A18,F18,A19,F19,A20,F20,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13], r14: Reader[A14,F14], r15: Reader[A15,F15], r16: Reader[A16,F16], r17: Reader[A17,F17], r18: Reader[A18,F18], r19: Reader[A19,F19], r20: Reader[A20,F20])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R, f14: A14 => R, f15: A15 => R, f16: A16 => R, f17: A17 => R, f18: A18 => R, f19: A19 => R, f20: A20 => R) = new Reader[R,S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      val bindr14 = r14.bind(s)
      val bindr15 = r15.bind(s)
      val bindr16 = r16.bind(s)
      val bindr17 = r17.bind(s)
      val bindr18 = r18.bind(s)
      val bindr19 = r19.bind(s)
      val bindr20 = r20.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case 14 => f14(bindr14.get)
        case 15 => f15(bindr15.get)
        case 16 => f16(bindr16.get)
        case 17 => f17(bindr17.get)
        case 18 => f18(bindr18.get)
        case 19 => f19(bindr19.get)
        case 20 => f20(bindr20.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union21R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13], a14: Reader[A,F14], a15: Reader[A,F15], a16: Reader[A,F16], a17: Reader[A,F17], a18: Reader[A,F18], a19: Reader[A,F19], a20: Reader[A,F20]): Reader[A,S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]] =
    s21R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)

  /** 22-way sum reader */
  def s22R[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,A18,F18,A19,F19,A20,F20,A21,F21,R](r0: Reader[A0,F0], r1: Reader[A1,F1], r2: Reader[A2,F2], r3: Reader[A3,F3], r4: Reader[A4,F4], r5: Reader[A5,F5], r6: Reader[A6,F6], r7: Reader[A7,F7], r8: Reader[A8,F8], r9: Reader[A9,F9], r10: Reader[A10,F10], r11: Reader[A11,F11], r12: Reader[A12,F12], r13: Reader[A13,F13], r14: Reader[A14,F14], r15: Reader[A15,F15], r16: Reader[A16,F16], r17: Reader[A17,F17], r18: Reader[A18,F18], r19: Reader[A19,F19], r20: Reader[A20,F20], r21: Reader[A21,F21])(f0: A0 => R, f1: A1 => R, f2: A2 => R, f3: A3 => R, f4: A4 => R, f5: A5 => R, f6: A6 => R, f7: A7 => R, f8: A8 => R, f9: A9 => R, f10: A10 => R, f11: A11 => R, f12: A12 => R, f13: A13 => R, f14: A14 => R, f15: A15 => R, f16: A16 => R, f17: A17 => R, f18: A18 => R, f19: A19 => R, f20: A20 => R, f21: A21 => R) = new Reader[R,S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]] {
    def bind(s: Source): Get[R] = new Get[R] {
      val bindr0 = r0.bind(s)
      val bindr1 = r1.bind(s)
      val bindr2 = r2.bind(s)
      val bindr3 = r3.bind(s)
      val bindr4 = r4.bind(s)
      val bindr5 = r5.bind(s)
      val bindr6 = r6.bind(s)
      val bindr7 = r7.bind(s)
      val bindr8 = r8.bind(s)
      val bindr9 = r9.bind(s)
      val bindr10 = r10.bind(s)
      val bindr11 = r11.bind(s)
      val bindr12 = r12.bind(s)
      val bindr13 = r13.bind(s)
      val bindr14 = r14.bind(s)
      val bindr15 = r15.bind(s)
      val bindr16 = r16.bind(s)
      val bindr17 = r17.bind(s)
      val bindr18 = r18.bind(s)
      val bindr19 = r19.bind(s)
      val bindr20 = r20.bind(s)
      val bindr21 = r21.bind(s)
      def get = (s.readByte: @scala.annotation.switch) match {
        case 0 => f0(bindr0.get)
        case 1 => f1(bindr1.get)
        case 2 => f2(bindr2.get)
        case 3 => f3(bindr3.get)
        case 4 => f4(bindr4.get)
        case 5 => f5(bindr5.get)
        case 6 => f6(bindr6.get)
        case 7 => f7(bindr7.get)
        case 8 => f8(bindr8.get)
        case 9 => f9(bindr9.get)
        case 10 => f10(bindr10.get)
        case 11 => f11(bindr11.get)
        case 12 => f12(bindr12.get)
        case 13 => f13(bindr13.get)
        case 14 => f14(bindr14.get)
        case 15 => f15(bindr15.get)
        case 16 => f16(bindr16.get)
        case 17 => f17(bindr17.get)
        case 18 => f18(bindr18.get)
        case 19 => f19(bindr19.get)
        case 20 => f20(bindr20.get)
        case 21 => f21(bindr21.get)
        case x => sys.error("unrecognized tag byte: " + x)
      }
    }
  }

  def union22R[A,F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21](a0: Reader[A,F0], a1: Reader[A,F1], a2: Reader[A,F2], a3: Reader[A,F3], a4: Reader[A,F4], a5: Reader[A,F5], a6: Reader[A,F6], a7: Reader[A,F7], a8: Reader[A,F8], a9: Reader[A,F9], a10: Reader[A,F10], a11: Reader[A,F11], a12: Reader[A,F12], a13: Reader[A,F13], a14: Reader[A,F14], a15: Reader[A,F15], a16: Reader[A,F16], a17: Reader[A,F17], a18: Reader[A,F18], a19: Reader[A,F19], a20: Reader[A,F20], a21: Reader[A,F21]): Reader[A,S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]] =
    s22R(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)(identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity, identity)
}