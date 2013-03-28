namespace f0

open ForceFormat

type P2<'a, 'b>(a:'a, b:'b) =
    interface Format with member f.Translate l = 
        l.TypeApplication("P2", [forceFormat(a).Translate l; forceFormat(b).Translate l])

type P3<'f1,'f2,'f3> = P2<'f1,P2<'f2,'f3>>
type P4<'f1,'f2,'f3,'f4> = P2<'f1,P2<'f2,P2<'f3,'f4>>>
type P5<'f1,'f2,'f3,'f4,'f5> = P2<'f1,P2<'f2,P2<'f3,P2<'f4,'f5>>>>
type P6<'f1,'f2,'f3,'f4,'f5,'f6> = P2<'f1,P2<'f2,P2<'f3,P2<'f4,P2<'f5,'f6>>>>>

type RepeatF<'a>(a: 'a) =
    interface Format with member f.Translate l = l.TypeApplication("RepeatF", [forceFormat(a).Translate l])
//TODO: Scala code has +A here for Alias...
type Alias<'a>(a: 'a) =
    interface Format with member f.Translate l = l.TypeApplication("AliasF", [forceFormat(a).Translate l])
type MemoF<'f>(f: 'f) =
    interface Format with member m.Translate l = l.TypeApplication("MemoF", [forceFormat(f).Translate l])
type FixF<'f>(f: 'f) =
    interface Format with member t.Translate l = l.TypeApplication("FixF", [forceFormat(f).Translate l])
type SelfF() = interface Format with member f.Translate l = "SelfF"
type ArgF() = interface Format with member f.Translate l = "ArgF"
type ApF<'f,'arg>(f:'f, a:'arg) =
    interface Format with member t.Translate l = 
        l.TypeApplication("ApF",  [forceFormat(f).Translate l; forceFormat(a).Translate l])
type DynamicF() = interface Format with member f.Translate l = "DynamicF"

type StreamF<'a> = FixF<S2<P2<'a,SelfF>, UnitF>>
type TreeF<'a> = FixF<P2<'a,RepeatF<SelfF>>>

module Formats =

    let unitF: UnitF = new UnitF()
    let shortF: ShortF = new ShortF()
    let intF: IntF = new IntF()
    let doubleF: DoubleF = new DoubleF()
    let floatF: FloatF = new FloatF()
    let stringF: StringF = new StringF()
    let booleanF: BooleanF = new BooleanF()
    let longF: LongF = new LongF()
    let byteF: ByteF = new ByteF()
    let selfF: SelfF = new SelfF()
    let argF: ArgF = new ArgF()
    let dynamicF: DynamicF = new DynamicF()

    let repeatF(a:'a) = new RepeatF<'a>(a)
    let p2F(a:'a, b:'b) = new P2<'a, 'b>(a, b)
    let p3F(a:'a, b:'b, c:'c) = p2F(a, p2F(b, c))
    let p4F(a:'a, b:'b, c:'c, d:'d) = p2F(a, p2F(b, p2F(c, d)))
    let p5F(a:'a, b:'b, c:'c, d:'d, e:'e) = p2F(a, p2F(b, p2F(c, p2F(d, e))))
    let p6F(a:'a, b:'b, c:'c, d:'d, e:'e, f:'f) = p2F(a, p2F(b, p2F(c, p2F(d, p2F(e, f)))))

(*** AUTO GENERATED CODE BELOW ***)
    let s2F(a0: 'a0, a1: 'a1) = new S2<'a0,'a1>(a0, a1)
    let s3F(a0: 'a0, a1: 'a1, a2: 'a2) = new S3<'a0,'a1,'a2>(a0, a1, a2)
    let s4F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3) = new S4<'a0,'a1,'a2,'a3>(a0, a1, a2, a3)
    let s5F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4) = new S5<'a0,'a1,'a2,'a3,'a4>(a0, a1, a2, a3, a4)
    let s6F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5) = new S6<'a0,'a1,'a2,'a3,'a4,'a5>(a0, a1, a2, a3, a4, a5)
    let s7F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6) = new S7<'a0,'a1,'a2,'a3,'a4,'a5,'a6>(a0, a1, a2, a3, a4, a5, a6)
    let s8F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7) = new S8<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7>(a0, a1, a2, a3, a4, a5, a6, a7)
    let s9F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8) = new S9<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8>(a0, a1, a2, a3, a4, a5, a6, a7, a8)
    let s10F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9) = new S10<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9)
    let s11F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10) = new S11<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
    let s12F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11) = new S12<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
    let s13F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12) = new S13<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
    let s14F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13) = new S14<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
    let s15F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14) = new S15<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
    let s16F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15) = new S16<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
    let s17F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16) = new S17<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
    let s18F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17) = new S18<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
    let s19F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17, a18: 'a18) = new S19<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17,'a18>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
    let s20F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17, a18: 'a18, a19: 'a19) = new S20<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17,'a18,'a19>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
    let s21F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17, a18: 'a18, a19: 'a19, a20: 'a20) = new S21<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17,'a18,'a19,'a20>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
    let s22F(a0: 'a0, a1: 'a1, a2: 'a2, a3: 'a3, a4: 'a4, a5: 'a5, a6: 'a6, a7: 'a7, a8: 'a8, a9: 'a9, a10: 'a10, a11: 'a11, a12: 'a12, a13: 'a13, a14: 'a14, a15: 'a15, a16: 'a16, a17: 'a17, a18: 'a18, a19: 'a19, a20: 'a20, a21: 'a21) = new S22<'a0,'a1,'a2,'a3,'a4,'a5,'a6,'a7,'a8,'a9,'a10,'a11,'a12,'a13,'a14,'a15,'a16,'a17,'a18,'a19,'a20,'a21>(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)

(*** AUTO GENERATED CODE ABOVE ***)

    let fixF(f:'f) = new FixF<'f>(f)
    let streamF(a: 'a): StreamF<'a> = fixF(s2F(p2F(a,selfF), unitF))