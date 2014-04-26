namespace f0

type Get<'a> =
    abstract member Get: 'a

[<AbstractClass>]
type Reader<'a,'f>() as self =
  abstract member Bind: Source -> Get<'a>
  member r.Map<'b>(f: 'a -> 'b): Reader<'b, 'f> = {
    new Reader<'b, 'f>() with
        member r.Bind(s:Source) =
            let outer = self.Bind(s)
            { new Get<'b> with member g.Get = f(outer.Get) }
  }
  member r.Mapcs (func : System.Func<'a,'b>) : Reader<'b,'f> = 
    self.Map (fun a -> func.Invoke(a))
  member r.FlatMap (f: 'a -> Reader<'b, 'f2>): Reader<'b, P2<'f, 'f2>> = {
    new Reader<'b, P2<'f, 'f2>>() with
        member r.Bind(s:Source) =
            let outer = self.Bind(s)
            { new Get<'b> with member g.Get = f(outer.Get).Bind(s).Get }
  }
  //member r.Erase: Reader<'a,System.Object> = r :> Reader<'a,System.Object>

type FixR<'a,'f>(f: Reader<'a,'f> -> Reader<'a,'f>) as self = 
    inherit Reader<'a,'f>()
    member this.fixedR = lazy(f self)
    override this.Bind(s:Source): Get<'a> = 
        let bound = lazy(self.fixedR.Force().Bind(s))
        { new Get<'a> with member g.Get = bound.Force().Get }
    
module Readers =

    let booleanF(s: Source): bool = s.ReadBit
    let byteF(s: Source): byte = s.ReadByte
    let bytesToInt16 (b1:byte) (b2:byte) : int16 = 
        ((0xFFs &&& (int16 b1)) <<< 8) ||| 
        ((0xFFs &&& (int16 b2)))
    let bytesToInt (b1:byte) (b2:byte) (b3:byte) (b4:byte) : int = 
        ((int b1) <<< 24) ||| 
        ((0xFF &&& (int b2)) <<< 16) ||| 
        ((0xFF &&& (int b3)) <<< 8) ||| 
        ((0xFF &&& (int b4)))
    let bytesToLong (b1:byte) (b2:byte) (b3:byte) (b4:byte) (b5:byte) (b6:byte) (b7:byte) (b8:byte) : int64 = 
        ((int64 b1) <<< 56) ||| 
        (((int64 0xFF) &&& (int64 b2)) <<< 48) ||| 
        (((int64 0xFF) &&& (int64 b3)) <<< 40) ||| 
        (((int64 0xFF) &&& (int64 b4)) <<< 32) ||| 
        (((int64 0xFF) &&& (int64 b5)) <<< 24) ||| 
        (((int64 0xFF) &&& (int64 b6)) <<< 16) ||| 
        (((int64 0xFF) &&& (int64 b7)) <<< 8) ||| 
        (((int64 0xFF) &&& (int64 b8)))
    let int16F(s: Source): int16 =
        bytesToInt16 (s.ReadByte) (s.ReadByte)
    let intF(s: Source): int =
        bytesToInt (s.ReadByte) (s.ReadByte) (s.ReadByte) (s.ReadByte)
    let longF(s: Source): int64 = 
        bytesToLong 
            (s.ReadByte) (s.ReadByte) (s.ReadByte) (s.ReadByte) 
            (s.ReadByte) (s.ReadByte) (s.ReadByte) (s.ReadByte)
    // TODO: double check these conversion functions
    let doubleF(s: Source) : double = System.BitConverter.Int64BitsToDouble(longF s)
    let singleF(s: Source) : single = System.BitConverter.ToSingle(System.BitConverter.GetBytes(intF s), 0)
    let stringF(s: Source) : string =
        let n = intF s
        let buf = s.ReadBytes n
        System.Text.UTF8Encoding().GetString(buf)

    let boolR: Reader<bool, BooleanF> = {
        new Reader<bool, BooleanF>() with 
            member r.Bind(s:Source) = { new Get<bool> with member g.Get = booleanF(s) } 
    }
    let byteR: Reader<byte, ByteF> = {
        new Reader<byte, ByteF>() with 
            member r.Bind(s:Source) = { new Get<byte> with member g.Get = byteF(s) } 
    }
    let shortR: Reader<int16, ShortF> = {
        new Reader<int16, ShortF>() with 
            member r.Bind(s:Source) = { new Get<int16> with member g.Get = int16F(s) } 
    }
    let intR: Reader<int, IntF> = {
        new Reader<int, IntF>() with
            member r.Bind(s:Source) = { new Get<int> with member g.Get = intF(s) } 
    }
    let longR: Reader<int64, LongF> = {
        new Reader<int64, LongF>() with
            member r.Bind(s:Source) = { new Get<int64> with member g.Get = longF(s) } 
    }
    let stringR: Reader<string, StringF> = {
        new Reader<string, StringF>() with
            member r.Bind(s:Source) = { new Get<string> with member g.Get = stringF(s) } 
    }
    let doubleR: Reader<double, DoubleF> = {
        new Reader<double, DoubleF>() with
            member r.Bind(s:Source) = { new Get<double> with member g.Get = doubleF(s) } 
    }
    let singleR: Reader<single, FloatF> = {
        new Reader<single, FloatF>() with
            member r.Bind(s:Source) = { new Get<single> with member g.Get = singleF(s) } 
    }
    type Nothing = | Nothing
    let unitR: Reader<Nothing, UnitF> = {
        new Reader<Nothing, UnitF>() with
            member r.Bind(s:Source) = { new Get<Nothing> with member g.Get = Nothing } 
    }
    let foldR(r:Reader<'a,'f>)(z:'b)(f: ('b * 'a) -> 'b): Reader<'b, RepeatF<'f>> = {
        new Reader<'b, RepeatF<'f>>() with 
            member this.Bind(s:Source) = 
                let elem = r.Bind s
                { 
                    new Get<'b> with member g.Get =
                        let mutable n = intF s
                        let mutable acc = z
                        while (n > 0) do
                            acc <- f(acc, elem.Get)
                            n <- n - 1
                        acc
                } 
    }
    let lazyStreamR(r: Reader<'a,'f>): Reader<Option<'a>,StreamF<'f>> = {
        new Reader<Option<'a>,StreamF<'f>>() with 
            member this.Bind(s:Source) = 
                let elem = r.Bind s
                { 
                    new Get<Option<'a>> with member g.Get =
                      if s.ReadBit then Some(elem.Get)
                      else None  
                }
    }
    let foldStreamR(r:Reader<'a,'f>)(z:'b)(f: ('b * 'a) -> 'b): Reader<'b, StreamF<'f>> = {
        new Reader<'b, StreamF<'f>>() with
            member this.Bind(s:Source) = 
                let elem = lazyStreamR(r).Bind(s)
                { 
                    new Get<'b> with member g.Get =
                        let mutable acc = z
                        let mutable cur = elem.Get
                        while (cur <> None) do
                            acc <- f(acc, cur.Value)
                            cur <- elem.Get
                        acc
                } 
    }

    type In<'a, 'b> = Reader<'a, 'b>

    let p2R(ra: Reader<'a,'f1>, rb: Reader<'b,'f2>)(f: ('a * 'b) -> 'r) = {
        new In<'r,P2<'f1, 'f2>>() with
            member r.Bind(s: Source): Get<'r> =
                let (a,b) = (ra.Bind(s),rb.Bind(s))
                { new Get<'r> with member g.Get = f(a.Get, b.Get) }
    }

    let p3R(ra: Reader<'a,'f1>, rb: Reader<'b,'f2>, rc: Reader<'c,'f3>)(f: ('a * 'b * 'c) -> 'r) = {
        new In<'r,P3<'f1,'f2,'f3>>() with
            member r.Bind(s: Source): Get<'r> = 
                let (a,b,c) = (ra.Bind(s),rb.Bind(s),rc.Bind(s))
                { new Get<'r> with member g.Get = f(a.Get, b.Get, c.Get) }
    }
    let p4R(ra: In<'a,'f1>, rb: In<'b,'f2>, rc: In<'c,'f3>, rd: In<'d,'f4>)(f: ('a * 'b * 'c * 'd) -> 'r) = {
        new In<'r,P4<'f1,'f2,'f3,'f4>>() with member r.Bind(s: Source): Get<'r> = {
            new Get<'r> with member g.Get =
                let (a,b,c,d) = (ra.Bind(s),rb.Bind(s),rc.Bind(s),rd.Bind(s))
                f(a.Get, b.Get, c.Get, d.Get)
        }
    }
    let p5R(ra: In<'a,'f1>, rb: In<'b,'f2>, rc: In<'c,'f3>, rd: In<'d,'f4>, re: In<'e, 'f5>)
           (f: ('a * 'b * 'c * 'd * 'e) -> 'r) = {
        new In<'r,P5<'f1,'f2,'f3,'f4,'f5>>() with member r.Bind(s: Source): Get<'r> = 
            let (a,b,c,d,e) = (ra.Bind(s),rb.Bind(s),rc.Bind(s),rd.Bind(s),re.Bind(s))
            { new Get<'r> with member g.Get = f(a.Get, b.Get, c.Get, d.Get, e.Get) }
    }
    let p6R(ra: In<'a,'f1>, rb: In<'b,'f2>, rc: In<'c,'f3>, rd: In<'d,'f4>, re: In<'e, 'f5>, rf: In<'f, 'f6>)
           (p: ('a * 'b * 'c * 'd * 'e * 'f) -> 'r) = {
        new In<'r,P6<'f1,'f2,'f3,'f4,'f5,'f6>>() with member r.Bind(s: Source): Get<'r> =
            let (a,b,c,d,e,f) = (ra.Bind(s),rb.Bind(s),rc.Bind(s),rd.Bind(s),re.Bind(s),rf.Bind(s))
            { new Get<'r> with member g.Get = p(a.Get, b.Get, c.Get, d.Get, e.Get, f.Get) }
    }

    let attemptR(r: Reader<'a,'f>) = {
        new Reader<Choice<System.Exception, 'a>,'f>() with member this.Bind(s: Source) =
            let elem = r.Bind(s)
            { new Get<Choice<System.Exception, 'a>> with member g.Get = 
                try Choice2Of2(elem.Get) with | e -> Choice1Of2(e)
            }
    }

    let tuple2R(ra,rb) = p2R(ra,rb)(id)
    let tuple3R(ra,rb,rc) = p3R(ra,rb,rc)(id)
    let tuple4R(ra,rb,rc,rd) = p4R(ra,rb,rc,rd)(id)
    let tuple5R(ra,rb,rc,rd,re) = p5R(ra,rb,rc,rd,re)(id)
    let tuple6R(ra,rb,rc,rd,re,rf) = p6R(ra,rb,rc,rd,re,rf)(id)
    let listR(r)  = (foldR(r)([])(fun (buf, a) -> a :: buf)).Map(List.rev)
    let streamR(r) = (foldStreamR(r)([])(fun (buf, a) -> a :: buf)).Map(List.rev)
    let fixR(f: Reader<'a,'f> -> Reader<'a,'f>) = new FixR<'a,'f>(f)

(*** AUTO GENERATED CODE BELOW ***)

    (* 2-way sum reader *)
    let s2R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r) = {
        new Reader<'r,S2<'f0,'f1>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union2R(r0,r1) = s2R(r0,r1)(id,id)

    (* 3-way sum reader *)
    let s3R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r) = {
        new Reader<'r,S3<'f0,'f1,'f2>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union3R(r0,r1,r2) = s3R(r0,r1,r2)(id,id,id)

    (* 4-way sum reader *)
    let s4R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r) = {
        new Reader<'r,S4<'f0,'f1,'f2,'f3>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union4R(r0,r1,r2,r3) = s4R(r0,r1,r2,r3)(id,id,id,id)

    (* 5-way sum reader *)
    let s5R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r) = {
        new Reader<'r,S5<'f0,'f1,'f2,'f3,'f4>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union5R(r0,r1,r2,r3,r4) = s5R(r0,r1,r2,r3,r4)(id,id,id,id,id)

    (* 6-way sum reader *)
    let s6R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r) = {
        new Reader<'r,S6<'f0,'f1,'f2,'f3,'f4,'f5>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union6R(r0,r1,r2,r3,r4,r5) = s6R(r0,r1,r2,r3,r4,r5)(id,id,id,id,id,id)

    (* 7-way sum reader *)
    let s7R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r) = {
        new Reader<'r,S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union7R(r0,r1,r2,r3,r4,r5,r6) = s7R(r0,r1,r2,r3,r4,r5,r6)(id,id,id,id,id,id,id)

    (* 8-way sum reader *)
    let s8R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r) = {
        new Reader<'r,S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union8R(r0,r1,r2,r3,r4,r5,r6,r7) = s8R(r0,r1,r2,r3,r4,r5,r6,r7)(id,id,id,id,id,id,id,id)

    (* 9-way sum reader *)
    let s9R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r) = {
        new Reader<'r,S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union9R(r0,r1,r2,r3,r4,r5,r6,r7,r8) = s9R(r0,r1,r2,r3,r4,r5,r6,r7,r8)(id,id,id,id,id,id,id,id,id)

    (* 10-way sum reader *)
    let s10R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r) = {
        new Reader<'r,S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union10R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9) = s10R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9)(id,id,id,id,id,id,id,id,id,id)

    (* 11-way sum reader *)
    let s11R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r) = {
        new Reader<'r,S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union11R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10) = s11R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)(id,id,id,id,id,id,id,id,id,id,id)

    (* 12-way sum reader *)
    let s12R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r) = {
        new Reader<'r,S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union12R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11) = s12R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11)(id,id,id,id,id,id,id,id,id,id,id,id)

    (* 13-way sum reader *)
    let s13R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r) = {
        new Reader<'r,S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union13R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12) = s13R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12)(id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 14-way sum reader *)
    let s14R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r) = {
        new Reader<'r,S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union14R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13) = s14R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13)(id,id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 15-way sum reader *)
    let s15R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>, r14: Reader<'a14,'f14>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r, f14: 'a14 -> 'r) = {
        new Reader<'r,S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            let r14Bound = r14.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | 14uy -> f14(r14Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union15R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14) = s15R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14)(id,id,id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 16-way sum reader *)
    let s16R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>, r14: Reader<'a14,'f14>, r15: Reader<'a15,'f15>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r, f14: 'a14 -> 'r, f15: 'a15 -> 'r) = {
        new Reader<'r,S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            let r14Bound = r14.Bind(s)
            let r15Bound = r15.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | 14uy -> f14(r14Bound.Get)
                | 15uy -> f15(r15Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union16R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15) = s16R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15)(id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 17-way sum reader *)
    let s17R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>, r14: Reader<'a14,'f14>, r15: Reader<'a15,'f15>, r16: Reader<'a16,'f16>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r, f14: 'a14 -> 'r, f15: 'a15 -> 'r, f16: 'a16 -> 'r) = {
        new Reader<'r,S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            let r14Bound = r14.Bind(s)
            let r15Bound = r15.Bind(s)
            let r16Bound = r16.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | 14uy -> f14(r14Bound.Get)
                | 15uy -> f15(r15Bound.Get)
                | 16uy -> f16(r16Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union17R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16) = s17R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16)(id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 18-way sum reader *)
    let s18R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>, r14: Reader<'a14,'f14>, r15: Reader<'a15,'f15>, r16: Reader<'a16,'f16>, r17: Reader<'a17,'f17>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r, f14: 'a14 -> 'r, f15: 'a15 -> 'r, f16: 'a16 -> 'r, f17: 'a17 -> 'r) = {
        new Reader<'r,S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            let r14Bound = r14.Bind(s)
            let r15Bound = r15.Bind(s)
            let r16Bound = r16.Bind(s)
            let r17Bound = r17.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | 14uy -> f14(r14Bound.Get)
                | 15uy -> f15(r15Bound.Get)
                | 16uy -> f16(r16Bound.Get)
                | 17uy -> f17(r17Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union18R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17) = s18R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17)(id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 19-way sum reader *)
    let s19R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>, r14: Reader<'a14,'f14>, r15: Reader<'a15,'f15>, r16: Reader<'a16,'f16>, r17: Reader<'a17,'f17>, r18: Reader<'a18,'f18>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r, f14: 'a14 -> 'r, f15: 'a15 -> 'r, f16: 'a16 -> 'r, f17: 'a17 -> 'r, f18: 'a18 -> 'r) = {
        new Reader<'r,S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            let r14Bound = r14.Bind(s)
            let r15Bound = r15.Bind(s)
            let r16Bound = r16.Bind(s)
            let r17Bound = r17.Bind(s)
            let r18Bound = r18.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | 14uy -> f14(r14Bound.Get)
                | 15uy -> f15(r15Bound.Get)
                | 16uy -> f16(r16Bound.Get)
                | 17uy -> f17(r17Bound.Get)
                | 18uy -> f18(r18Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union19R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18) = s19R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18)(id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 20-way sum reader *)
    let s20R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>, r14: Reader<'a14,'f14>, r15: Reader<'a15,'f15>, r16: Reader<'a16,'f16>, r17: Reader<'a17,'f17>, r18: Reader<'a18,'f18>, r19: Reader<'a19,'f19>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r, f14: 'a14 -> 'r, f15: 'a15 -> 'r, f16: 'a16 -> 'r, f17: 'a17 -> 'r, f18: 'a18 -> 'r, f19: 'a19 -> 'r) = {
        new Reader<'r,S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            let r14Bound = r14.Bind(s)
            let r15Bound = r15.Bind(s)
            let r16Bound = r16.Bind(s)
            let r17Bound = r17.Bind(s)
            let r18Bound = r18.Bind(s)
            let r19Bound = r19.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | 14uy -> f14(r14Bound.Get)
                | 15uy -> f15(r15Bound.Get)
                | 16uy -> f16(r16Bound.Get)
                | 17uy -> f17(r17Bound.Get)
                | 18uy -> f18(r18Bound.Get)
                | 19uy -> f19(r19Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union20R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19) = s20R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19)(id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 21-way sum reader *)
    let s21R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>, r14: Reader<'a14,'f14>, r15: Reader<'a15,'f15>, r16: Reader<'a16,'f16>, r17: Reader<'a17,'f17>, r18: Reader<'a18,'f18>, r19: Reader<'a19,'f19>, r20: Reader<'a20,'f20>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r, f14: 'a14 -> 'r, f15: 'a15 -> 'r, f16: 'a16 -> 'r, f17: 'a17 -> 'r, f18: 'a18 -> 'r, f19: 'a19 -> 'r, f20: 'a20 -> 'r) = {
        new Reader<'r,S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            let r14Bound = r14.Bind(s)
            let r15Bound = r15.Bind(s)
            let r16Bound = r16.Bind(s)
            let r17Bound = r17.Bind(s)
            let r18Bound = r18.Bind(s)
            let r19Bound = r19.Bind(s)
            let r20Bound = r20.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | 14uy -> f14(r14Bound.Get)
                | 15uy -> f15(r15Bound.Get)
                | 16uy -> f16(r16Bound.Get)
                | 17uy -> f17(r17Bound.Get)
                | 18uy -> f18(r18Bound.Get)
                | 19uy -> f19(r19Bound.Get)
                | 20uy -> f20(r20Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union21R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20) = s21R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20)(id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id)

    (* 22-way sum reader *)
    let s22R(r0: Reader<'a0,'f0>, r1: Reader<'a1,'f1>, r2: Reader<'a2,'f2>, r3: Reader<'a3,'f3>, r4: Reader<'a4,'f4>, r5: Reader<'a5,'f5>, r6: Reader<'a6,'f6>, r7: Reader<'a7,'f7>, r8: Reader<'a8,'f8>, r9: Reader<'a9,'f9>, r10: Reader<'a10,'f10>, r11: Reader<'a11,'f11>, r12: Reader<'a12,'f12>, r13: Reader<'a13,'f13>, r14: Reader<'a14,'f14>, r15: Reader<'a15,'f15>, r16: Reader<'a16,'f16>, r17: Reader<'a17,'f17>, r18: Reader<'a18,'f18>, r19: Reader<'a19,'f19>, r20: Reader<'a20,'f20>, r21: Reader<'a21,'f21>)
            (f0: 'a0 -> 'r, f1: 'a1 -> 'r, f2: 'a2 -> 'r, f3: 'a3 -> 'r, f4: 'a4 -> 'r, f5: 'a5 -> 'r, f6: 'a6 -> 'r, f7: 'a7 -> 'r, f8: 'a8 -> 'r, f9: 'a9 -> 'r, f10: 'a10 -> 'r, f11: 'a11 -> 'r, f12: 'a12 -> 'r, f13: 'a13 -> 'r, f14: 'a14 -> 'r, f15: 'a15 -> 'r, f16: 'a16 -> 'r, f17: 'a17 -> 'r, f18: 'a18 -> 'r, f19: 'a19 -> 'r, f20: 'a20 -> 'r, f21: 'a21 -> 'r) = {
        new Reader<'r,S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>() with member r.Bind(s: Source) =
            let r0Bound = r0.Bind(s)
            let r1Bound = r1.Bind(s)
            let r2Bound = r2.Bind(s)
            let r3Bound = r3.Bind(s)
            let r4Bound = r4.Bind(s)
            let r5Bound = r5.Bind(s)
            let r6Bound = r6.Bind(s)
            let r7Bound = r7.Bind(s)
            let r8Bound = r8.Bind(s)
            let r9Bound = r9.Bind(s)
            let r10Bound = r10.Bind(s)
            let r11Bound = r11.Bind(s)
            let r12Bound = r12.Bind(s)
            let r13Bound = r13.Bind(s)
            let r14Bound = r14.Bind(s)
            let r15Bound = r15.Bind(s)
            let r16Bound = r16.Bind(s)
            let r17Bound = r17.Bind(s)
            let r18Bound = r18.Bind(s)
            let r19Bound = r19.Bind(s)
            let r20Bound = r20.Bind(s)
            let r21Bound = r21.Bind(s)
            { new Get<'r> with member g.Get =
                match byteF(s) with
                | 0uy -> f0(r0Bound.Get)
                | 1uy -> f1(r1Bound.Get)
                | 2uy -> f2(r2Bound.Get)
                | 3uy -> f3(r3Bound.Get)
                | 4uy -> f4(r4Bound.Get)
                | 5uy -> f5(r5Bound.Get)
                | 6uy -> f6(r6Bound.Get)
                | 7uy -> f7(r7Bound.Get)
                | 8uy -> f8(r8Bound.Get)
                | 9uy -> f9(r9Bound.Get)
                | 10uy -> f10(r10Bound.Get)
                | 11uy -> f11(r11Bound.Get)
                | 12uy -> f12(r12Bound.Get)
                | 13uy -> f13(r13Bound.Get)
                | 14uy -> f14(r14Bound.Get)
                | 15uy -> f15(r15Bound.Get)
                | 16uy -> f16(r16Bound.Get)
                | 17uy -> f17(r17Bound.Get)
                | 18uy -> f18(r18Bound.Get)
                | 19uy -> f19(r19Bound.Get)
                | 20uy -> f20(r20Bound.Get)
                | 21uy -> f21(r21Bound.Get)
                | x -> failwith("unrecognized tag byte: " + x.ToString())
            }
    }

    let union22R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21) = s22R(r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21)(id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id,id)

(*** AUTO GENERATED CODE ABOVE ***)

    let optionR(ra) = s2R(unitR, ra)((fun _ -> None),Some)
    let choiceR(ra,rb): Reader<Choice<'a,'b>,S2<'f1,'f2>> = s2R(ra,rb)(Choice1Of2, Choice2Of2)    