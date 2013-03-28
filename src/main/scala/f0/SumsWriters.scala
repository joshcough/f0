package f0

import Formats._

trait SumWriters {
    
  /** 2-way sum writer */
  def s2W[A0,F0,A1,F1,R](w0: Writer[A0,F0], w1: Writer[A1,F1])
                (f: (A0 => EffectW[S2[F0,F1]], A1 => EffectW[S2[F0,F1]]) => R => EffectW[S2[F0,F1]])
  = new Writer[R,S2[F0,F1]] {
    def bind(o: Sink): R => EffectW[S2[F0,F1]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S2[F0,F1]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S2[F0,F1]]] }) }
      f(bindw0, bindw1)
    }}
    

  /** 3-way sum writer */
  def s3W[A0,F0,A1,F1,A2,F2,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2])
                (f: (A0 => EffectW[S3[F0,F1,F2]], A1 => EffectW[S3[F0,F1,F2]], A2 => EffectW[S3[F0,F1,F2]]) => R => EffectW[S3[F0,F1,F2]])
  = new Writer[R,S3[F0,F1,F2]] {
    def bind(o: Sink): R => EffectW[S3[F0,F1,F2]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S3[F0,F1,F2]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S3[F0,F1,F2]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S3[F0,F1,F2]]] }) }
      f(bindw0, bindw1, bindw2)
    }}
    

  /** 4-way sum writer */
  def s4W[A0,F0,A1,F1,A2,F2,A3,F3,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3])
                (f: (A0 => EffectW[S4[F0,F1,F2,F3]], A1 => EffectW[S4[F0,F1,F2,F3]], A2 => EffectW[S4[F0,F1,F2,F3]], A3 => EffectW[S4[F0,F1,F2,F3]]) => R => EffectW[S4[F0,F1,F2,F3]])
  = new Writer[R,S4[F0,F1,F2,F3]] {
    def bind(o: Sink): R => EffectW[S4[F0,F1,F2,F3]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S4[F0,F1,F2,F3]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S4[F0,F1,F2,F3]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S4[F0,F1,F2,F3]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S4[F0,F1,F2,F3]]] }) }
      f(bindw0, bindw1, bindw2, bindw3)
    }}
    

  /** 5-way sum writer */
  def s5W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4])
                (f: (A0 => EffectW[S5[F0,F1,F2,F3,F4]], A1 => EffectW[S5[F0,F1,F2,F3,F4]], A2 => EffectW[S5[F0,F1,F2,F3,F4]], A3 => EffectW[S5[F0,F1,F2,F3,F4]], A4 => EffectW[S5[F0,F1,F2,F3,F4]]) => R => EffectW[S5[F0,F1,F2,F3,F4]])
  = new Writer[R,S5[F0,F1,F2,F3,F4]] {
    def bind(o: Sink): R => EffectW[S5[F0,F1,F2,F3,F4]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S5[F0,F1,F2,F3,F4]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S5[F0,F1,F2,F3,F4]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S5[F0,F1,F2,F3,F4]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S5[F0,F1,F2,F3,F4]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S5[F0,F1,F2,F3,F4]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4)
    }}
    

  /** 6-way sum writer */
  def s6W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5])
                (f: (A0 => EffectW[S6[F0,F1,F2,F3,F4,F5]], A1 => EffectW[S6[F0,F1,F2,F3,F4,F5]], A2 => EffectW[S6[F0,F1,F2,F3,F4,F5]], A3 => EffectW[S6[F0,F1,F2,F3,F4,F5]], A4 => EffectW[S6[F0,F1,F2,F3,F4,F5]], A5 => EffectW[S6[F0,F1,F2,F3,F4,F5]]) => R => EffectW[S6[F0,F1,F2,F3,F4,F5]])
  = new Writer[R,S6[F0,F1,F2,F3,F4,F5]] {
    def bind(o: Sink): R => EffectW[S6[F0,F1,F2,F3,F4,F5]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S6[F0,F1,F2,F3,F4,F5]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S6[F0,F1,F2,F3,F4,F5]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S6[F0,F1,F2,F3,F4,F5]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S6[F0,F1,F2,F3,F4,F5]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S6[F0,F1,F2,F3,F4,F5]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S6[F0,F1,F2,F3,F4,F5]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5)
    }}
    

  /** 7-way sum writer */
  def s7W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6])
                (f: (A0 => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]], A1 => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]], A2 => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]], A3 => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]], A4 => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]], A5 => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]], A6 => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]]) => R => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]])
  = new Writer[R,S7[F0,F1,F2,F3,F4,F5,F6]] {
    def bind(o: Sink): R => EffectW[S7[F0,F1,F2,F3,F4,F5,F6]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S7[F0,F1,F2,F3,F4,F5,F6]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S7[F0,F1,F2,F3,F4,F5,F6]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S7[F0,F1,F2,F3,F4,F5,F6]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S7[F0,F1,F2,F3,F4,F5,F6]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S7[F0,F1,F2,F3,F4,F5,F6]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S7[F0,F1,F2,F3,F4,F5,F6]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S7[F0,F1,F2,F3,F4,F5,F6]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6)
    }}
    

  /** 8-way sum writer */
  def s8W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7])
                (f: (A0 => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]], A1 => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]], A2 => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]], A3 => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]], A4 => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]], A5 => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]], A6 => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]], A7 => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]) => R => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]])
  = new Writer[R,S8[F0,F1,F2,F3,F4,F5,F6,F7]] {
    def bind(o: Sink): R => EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S8[F0,F1,F2,F3,F4,F5,F6,F7]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7)
    }}
    

  /** 9-way sum writer */
  def s9W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8])
                (f: (A0 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]], A1 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]], A2 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]], A3 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]], A4 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]], A5 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]], A6 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]], A7 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]], A8 => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]) => R => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]])
  = new Writer[R,S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]] {
    def bind(o: Sink): R => EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S9[F0,F1,F2,F3,F4,F5,F6,F7,F8]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8)
    }}
    

  /** 10-way sum writer */
  def s10W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9])
                (f: (A0 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A1 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A2 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A3 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A4 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A5 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A6 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A7 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A8 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]], A9 => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]) => R => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]])
  = new Writer[R,S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]] {
    def bind(o: Sink): R => EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S10[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9)
    }}
    

  /** 11-way sum writer */
  def s11W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10])
                (f: (A0 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A1 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A2 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A3 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A4 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A5 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A6 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A7 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A8 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A9 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]], A10 => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]) => R => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]])
  = new Writer[R,S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]] {
    def bind(o: Sink): R => EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S11[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10)
    }}
    

  /** 12-way sum writer */
  def s12W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11])
                (f: (A0 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A1 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A2 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A3 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A4 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A5 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A6 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A7 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A8 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A9 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A10 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]], A11 => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]) => R => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]])
  = new Writer[R,S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]] {
    def bind(o: Sink): R => EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S12[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11)
    }}
    

  /** 13-way sum writer */
  def s13W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12])
                (f: (A0 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A1 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A2 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A3 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A4 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A5 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A6 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A7 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A8 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A9 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A10 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A11 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]], A12 => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]) => R => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]])
  = new Writer[R,S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]] {
    def bind(o: Sink): R => EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S13[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12)
    }}
    

  /** 14-way sum writer */
  def s14W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13])
                (f: (A0 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A1 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A2 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A3 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A4 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A5 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A6 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A7 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A8 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A9 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A10 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A11 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A12 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]], A13 => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]) => R => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]])
  = new Writer[R,S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]] {
    def bind(o: Sink): R => EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S14[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13)
    }}
    

  /** 15-way sum writer */
  def s15W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13], w14: Writer[A14,F14])
                (f: (A0 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A1 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A2 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A3 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A4 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A5 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A6 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A7 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A8 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A9 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A10 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A11 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A12 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A13 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]], A14 => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]) => R => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]])
  = new Writer[R,S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]] {
    def bind(o: Sink): R => EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      val bindw14 = { val b = w14.bind(o); ((a: A14) => { o(14:Byte); b(a).asInstanceOf[EffectW[S15[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13, bindw14)
    }}
    

  /** 16-way sum writer */
  def s16W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13], w14: Writer[A14,F14], w15: Writer[A15,F15])
                (f: (A0 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A1 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A2 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A3 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A4 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A5 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A6 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A7 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A8 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A9 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A10 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A11 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A12 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A13 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A14 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]], A15 => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]) => R => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]])
  = new Writer[R,S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]] {
    def bind(o: Sink): R => EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw14 = { val b = w14.bind(o); ((a: A14) => { o(14:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      val bindw15 = { val b = w15.bind(o); ((a: A15) => { o(15:Byte); b(a).asInstanceOf[EffectW[S16[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13, bindw14, bindw15)
    }}
    

  /** 17-way sum writer */
  def s17W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13], w14: Writer[A14,F14], w15: Writer[A15,F15], w16: Writer[A16,F16])
                (f: (A0 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A1 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A2 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A3 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A4 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A5 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A6 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A7 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A8 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A9 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A10 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A11 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A12 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A13 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A14 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A15 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]], A16 => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]) => R => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]])
  = new Writer[R,S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]] {
    def bind(o: Sink): R => EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw14 = { val b = w14.bind(o); ((a: A14) => { o(14:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw15 = { val b = w15.bind(o); ((a: A15) => { o(15:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      val bindw16 = { val b = w16.bind(o); ((a: A16) => { o(16:Byte); b(a).asInstanceOf[EffectW[S17[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13, bindw14, bindw15, bindw16)
    }}
    

  /** 18-way sum writer */
  def s18W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13], w14: Writer[A14,F14], w15: Writer[A15,F15], w16: Writer[A16,F16], w17: Writer[A17,F17])
                (f: (A0 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A1 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A2 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A3 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A4 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A5 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A6 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A7 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A8 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A9 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A10 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A11 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A12 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A13 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A14 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A15 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A16 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]], A17 => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]) => R => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]])
  = new Writer[R,S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]] {
    def bind(o: Sink): R => EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw14 = { val b = w14.bind(o); ((a: A14) => { o(14:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw15 = { val b = w15.bind(o); ((a: A15) => { o(15:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw16 = { val b = w16.bind(o); ((a: A16) => { o(16:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      val bindw17 = { val b = w17.bind(o); ((a: A17) => { o(17:Byte); b(a).asInstanceOf[EffectW[S18[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13, bindw14, bindw15, bindw16, bindw17)
    }}
    

  /** 19-way sum writer */
  def s19W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,A18,F18,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13], w14: Writer[A14,F14], w15: Writer[A15,F15], w16: Writer[A16,F16], w17: Writer[A17,F17], w18: Writer[A18,F18])
                (f: (A0 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A1 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A2 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A3 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A4 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A5 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A6 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A7 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A8 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A9 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A10 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A11 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A12 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A13 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A14 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A15 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A16 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A17 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]], A18 => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]) => R => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]])
  = new Writer[R,S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]] {
    def bind(o: Sink): R => EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw14 = { val b = w14.bind(o); ((a: A14) => { o(14:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw15 = { val b = w15.bind(o); ((a: A15) => { o(15:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw16 = { val b = w16.bind(o); ((a: A16) => { o(16:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw17 = { val b = w17.bind(o); ((a: A17) => { o(17:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      val bindw18 = { val b = w18.bind(o); ((a: A18) => { o(18:Byte); b(a).asInstanceOf[EffectW[S19[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13, bindw14, bindw15, bindw16, bindw17, bindw18)
    }}
    

  /** 20-way sum writer */
  def s20W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,A18,F18,A19,F19,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13], w14: Writer[A14,F14], w15: Writer[A15,F15], w16: Writer[A16,F16], w17: Writer[A17,F17], w18: Writer[A18,F18], w19: Writer[A19,F19])
                (f: (A0 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A1 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A2 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A3 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A4 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A5 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A6 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A7 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A8 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A9 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A10 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A11 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A12 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A13 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A14 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A15 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A16 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A17 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A18 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]], A19 => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]) => R => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]])
  = new Writer[R,S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]] {
    def bind(o: Sink): R => EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw14 = { val b = w14.bind(o); ((a: A14) => { o(14:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw15 = { val b = w15.bind(o); ((a: A15) => { o(15:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw16 = { val b = w16.bind(o); ((a: A16) => { o(16:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw17 = { val b = w17.bind(o); ((a: A17) => { o(17:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw18 = { val b = w18.bind(o); ((a: A18) => { o(18:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      val bindw19 = { val b = w19.bind(o); ((a: A19) => { o(19:Byte); b(a).asInstanceOf[EffectW[S20[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13, bindw14, bindw15, bindw16, bindw17, bindw18, bindw19)
    }}
    

  /** 21-way sum writer */
  def s21W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,A18,F18,A19,F19,A20,F20,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13], w14: Writer[A14,F14], w15: Writer[A15,F15], w16: Writer[A16,F16], w17: Writer[A17,F17], w18: Writer[A18,F18], w19: Writer[A19,F19], w20: Writer[A20,F20])
                (f: (A0 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A1 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A2 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A3 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A4 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A5 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A6 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A7 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A8 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A9 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A10 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A11 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A12 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A13 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A14 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A15 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A16 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A17 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A18 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A19 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]], A20 => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]) => R => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]])
  = new Writer[R,S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]] {
    def bind(o: Sink): R => EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw14 = { val b = w14.bind(o); ((a: A14) => { o(14:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw15 = { val b = w15.bind(o); ((a: A15) => { o(15:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw16 = { val b = w16.bind(o); ((a: A16) => { o(16:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw17 = { val b = w17.bind(o); ((a: A17) => { o(17:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw18 = { val b = w18.bind(o); ((a: A18) => { o(18:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw19 = { val b = w19.bind(o); ((a: A19) => { o(19:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      val bindw20 = { val b = w20.bind(o); ((a: A20) => { o(20:Byte); b(a).asInstanceOf[EffectW[S21[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13, bindw14, bindw15, bindw16, bindw17, bindw18, bindw19, bindw20)
    }}
    

  /** 22-way sum writer */
  def s22W[A0,F0,A1,F1,A2,F2,A3,F3,A4,F4,A5,F5,A6,F6,A7,F7,A8,F8,A9,F9,A10,F10,A11,F11,A12,F12,A13,F13,A14,F14,A15,F15,A16,F16,A17,F17,A18,F18,A19,F19,A20,F20,A21,F21,R](w0: Writer[A0,F0], w1: Writer[A1,F1], w2: Writer[A2,F2], w3: Writer[A3,F3], w4: Writer[A4,F4], w5: Writer[A5,F5], w6: Writer[A6,F6], w7: Writer[A7,F7], w8: Writer[A8,F8], w9: Writer[A9,F9], w10: Writer[A10,F10], w11: Writer[A11,F11], w12: Writer[A12,F12], w13: Writer[A13,F13], w14: Writer[A14,F14], w15: Writer[A15,F15], w16: Writer[A16,F16], w17: Writer[A17,F17], w18: Writer[A18,F18], w19: Writer[A19,F19], w20: Writer[A20,F20], w21: Writer[A21,F21])
                (f: (A0 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A1 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A2 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A3 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A4 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A5 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A6 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A7 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A8 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A9 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A10 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A11 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A12 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A13 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A14 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A15 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A16 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A17 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A18 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A19 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A20 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]], A21 => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]) => R => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]])
  = new Writer[R,S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]] {
    def bind(o: Sink): R => EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]] = {
      val bindw0 = { val b = w0.bind(o); ((a: A0) => { o(0:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw1 = { val b = w1.bind(o); ((a: A1) => { o(1:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw2 = { val b = w2.bind(o); ((a: A2) => { o(2:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw3 = { val b = w3.bind(o); ((a: A3) => { o(3:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw4 = { val b = w4.bind(o); ((a: A4) => { o(4:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw5 = { val b = w5.bind(o); ((a: A5) => { o(5:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw6 = { val b = w6.bind(o); ((a: A6) => { o(6:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw7 = { val b = w7.bind(o); ((a: A7) => { o(7:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw8 = { val b = w8.bind(o); ((a: A8) => { o(8:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw9 = { val b = w9.bind(o); ((a: A9) => { o(9:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw10 = { val b = w10.bind(o); ((a: A10) => { o(10:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw11 = { val b = w11.bind(o); ((a: A11) => { o(11:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw12 = { val b = w12.bind(o); ((a: A12) => { o(12:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw13 = { val b = w13.bind(o); ((a: A13) => { o(13:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw14 = { val b = w14.bind(o); ((a: A14) => { o(14:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw15 = { val b = w15.bind(o); ((a: A15) => { o(15:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw16 = { val b = w16.bind(o); ((a: A16) => { o(16:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw17 = { val b = w17.bind(o); ((a: A17) => { o(17:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw18 = { val b = w18.bind(o); ((a: A18) => { o(18:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw19 = { val b = w19.bind(o); ((a: A19) => { o(19:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw20 = { val b = w20.bind(o); ((a: A20) => { o(20:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      val bindw21 = { val b = w21.bind(o); ((a: A21) => { o(21:Byte); b(a).asInstanceOf[EffectW[S22[F0,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21]]] }) }
      f(bindw0, bindw1, bindw2, bindw3, bindw4, bindw5, bindw6, bindw7, bindw8, bindw9, bindw10, bindw11, bindw12, bindw13, bindw14, bindw15, bindw16, bindw17, bindw18, bindw19, bindw20, bindw21)
    }}
    
}