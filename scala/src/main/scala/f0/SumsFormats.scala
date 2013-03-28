package f0

import Formats.Implicits.asFormat
    
case class S2[+A0,+A1](a0: A0, a1: A1) extends Format {
  override def translate(l: Language) = l.typeApplication("S2", a0 translate l, a1 translate l)
}

case class S3[+A0,+A1,+A2](a0: A0, a1: A1, a2: A2) extends Format {
  override def translate(l: Language) = l.typeApplication("S3", a0 translate l, a1 translate l, a2 translate l)
}

case class S4[+A0,+A1,+A2,+A3](a0: A0, a1: A1, a2: A2, a3: A3) extends Format {
  override def translate(l: Language) = l.typeApplication("S4", a0 translate l, a1 translate l, a2 translate l, a3 translate l)
}

case class S5[+A0,+A1,+A2,+A3,+A4](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4) extends Format {
  override def translate(l: Language) = l.typeApplication("S5", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l)
}

case class S6[+A0,+A1,+A2,+A3,+A4,+A5](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5) extends Format {
  override def translate(l: Language) = l.typeApplication("S6", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l)
}

case class S7[+A0,+A1,+A2,+A3,+A4,+A5,+A6](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6) extends Format {
  override def translate(l: Language) = l.typeApplication("S7", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l)
}

case class S8[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7) extends Format {
  override def translate(l: Language) = l.typeApplication("S8", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l)
}

case class S9[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8) extends Format {
  override def translate(l: Language) = l.typeApplication("S9", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l)
}

case class S10[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9) extends Format {
  override def translate(l: Language) = l.typeApplication("S10", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l)
}

case class S11[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10) extends Format {
  override def translate(l: Language) = l.typeApplication("S11", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l)
}

case class S12[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11) extends Format {
  override def translate(l: Language) = l.typeApplication("S12", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l)
}

case class S13[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12) extends Format {
  override def translate(l: Language) = l.typeApplication("S13", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l)
}

case class S14[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13) extends Format {
  override def translate(l: Language) = l.typeApplication("S14", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l)
}

case class S15[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13,+A14](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14) extends Format {
  override def translate(l: Language) = l.typeApplication("S15", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l, a14 translate l)
}

case class S16[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13,+A14,+A15](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15) extends Format {
  override def translate(l: Language) = l.typeApplication("S16", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l, a14 translate l, a15 translate l)
}

case class S17[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13,+A14,+A15,+A16](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16) extends Format {
  override def translate(l: Language) = l.typeApplication("S17", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l, a14 translate l, a15 translate l, a16 translate l)
}

case class S18[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13,+A14,+A15,+A16,+A17](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17) extends Format {
  override def translate(l: Language) = l.typeApplication("S18", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l, a14 translate l, a15 translate l, a16 translate l, a17 translate l)
}

case class S19[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13,+A14,+A15,+A16,+A17,+A18](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18) extends Format {
  override def translate(l: Language) = l.typeApplication("S19", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l, a14 translate l, a15 translate l, a16 translate l, a17 translate l, a18 translate l)
}

case class S20[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13,+A14,+A15,+A16,+A17,+A18,+A19](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19) extends Format {
  override def translate(l: Language) = l.typeApplication("S20", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l, a14 translate l, a15 translate l, a16 translate l, a17 translate l, a18 translate l, a19 translate l)
}

case class S21[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13,+A14,+A15,+A16,+A17,+A18,+A19,+A20](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20) extends Format {
  override def translate(l: Language) = l.typeApplication("S21", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l, a14 translate l, a15 translate l, a16 translate l, a17 translate l, a18 translate l, a19 translate l, a20 translate l)
}

case class S22[+A0,+A1,+A2,+A3,+A4,+A5,+A6,+A7,+A8,+A9,+A10,+A11,+A12,+A13,+A14,+A15,+A16,+A17,+A18,+A19,+A20,+A21](a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20, a21: A21) extends Format {
  override def translate(l: Language) = l.typeApplication("S22", a0 translate l, a1 translate l, a2 translate l, a3 translate l, a4 translate l, a5 translate l, a6 translate l, a7 translate l, a8 translate l, a9 translate l, a10 translate l, a11 translate l, a12 translate l, a13 translate l, a14 translate l, a15 translate l, a16 translate l, a17 translate l, a18 translate l, a19 translate l, a20 translate l, a21 translate l)
}
trait SumFormats {
  implicit def s2F[A0,A1](implicit a0: A0, a1: A1) = S2(a0, a1)
  implicit def s3F[A0,A1,A2](implicit a0: A0, a1: A1, a2: A2) = S3(a0, a1, a2)
  implicit def s4F[A0,A1,A2,A3](implicit a0: A0, a1: A1, a2: A2, a3: A3) = S4(a0, a1, a2, a3)
  implicit def s5F[A0,A1,A2,A3,A4](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4) = S5(a0, a1, a2, a3, a4)
  implicit def s6F[A0,A1,A2,A3,A4,A5](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5) = S6(a0, a1, a2, a3, a4, a5)
  implicit def s7F[A0,A1,A2,A3,A4,A5,A6](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6) = S7(a0, a1, a2, a3, a4, a5, a6)
  implicit def s8F[A0,A1,A2,A3,A4,A5,A6,A7](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7) = S8(a0, a1, a2, a3, a4, a5, a6, a7)
  implicit def s9F[A0,A1,A2,A3,A4,A5,A6,A7,A8](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8) = S9(a0, a1, a2, a3, a4, a5, a6, a7, a8)
  implicit def s10F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9) = S10(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
  implicit def s11F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10) = S11(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  implicit def s12F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11) = S12(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
  implicit def s13F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12) = S13(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
  implicit def s14F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13) = S14(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
  implicit def s15F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14) = S15(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
  implicit def s16F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15) = S16(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
  implicit def s17F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16) = S17(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  implicit def s18F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17) = S18(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
  implicit def s19F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18) = S19(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
  implicit def s20F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19) = S20(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
  implicit def s21F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20) = S21(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
  implicit def s22F[A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21](implicit a0: A0, a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9, a10: A10, a11: A11, a12: A12, a13: A13, a14: A14, a15: A15, a16: A16, a17: A17, a18: A18, a19: A19, a20: A20, a21: A21) = S22(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
}