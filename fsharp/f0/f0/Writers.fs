namespace f0

open Effects
open Helpers

[<AbstractClass>]
type Writer<'a,'f>() as self =
  abstract member Format: 'f
  abstract member Bind: Sink -> ('a -> EffectW<'f>)
  member r.Cmap(f: 'a0 -> 'a): Writer<'a0,'f> = {
    new Writer<'a0,'f>() with
        member w.Format = self.Format 
        member w.Bind(out:Sink) = f >> self.Bind(out)
  }
  member r.Cmapcs (func : System.Func<'a0,'a>) : Writer<'a0,'f> = 
    self.Cmap (fun a -> func.Invoke(a))

type FixW<'a, 'f>(f: Writer<'a,'f> -> Writer<'a,'f>) as self =
    inherit Writer<'a,'f>()
    member this.fixedW = lazy(f self)
    override this.Format = failwith("todo")
    override this.Bind(out:Sink): ('a -> EffectW<'f>) =
        let bound = lazy(self.fixedW.Force().Bind(out))
        (fun a -> bound.Force() a)

module Writers =

    let boolF (b: bool)(o: Sink): EffectW<BooleanF> = o.WriteBit(b); ew
    let byteF (b: byte)(o: Sink): EffectW<ByteF>    = o.WriteByte(b); ew
    let shortF(i: int16)(o: Sink): EffectW<ShortF> =
        o.WriteByte(byte (i >>>  8))
        o.WriteByte(byte (i >>>  0))
        ew
    let intF  (i: int )(o: Sink): EffectW<IntF> =
        o.WriteByte(byte (i >>> 24))
        o.WriteByte(byte (i >>> 16))
        o.WriteByte(byte (i >>>  8))
        o.WriteByte(byte (i >>>  0))
        ew
    let longF (l: int64)(o: Sink): EffectW<LongF> =
        o.WriteByte(byte (l >>> 56))
        o.WriteByte(byte (l >>> 48))
        o.WriteByte(byte (l >>> 40))
        o.WriteByte(byte (l >>> 32))
        o.WriteByte(byte (l >>> 24))
        o.WriteByte(byte (l >>> 16))
        o.WriteByte(byte (l >>>  8))
        o.WriteByte(byte (l >>>  0))
        ew
    let doubleF(d:double)(o: Sink) : EffectW<DoubleF> =
        let _ = longF (System.BitConverter.DoubleToInt64Bits(d)) o
        ew
    let singleF(f:single)(o: Sink) : EffectW<FloatF> =
        let _ = intF (System.BitConverter.ToInt32(System.BitConverter.GetBytes(f),0)) o
        ew
    let stringF(s: string)(o: Sink): EffectW<StringF> =
        let buf = System.Text.UTF8Encoding().GetBytes(s)
        let mutable i = 0
        let _ = intF buf.Length o
        while (i < buf.Length) do 
            o.WriteByte(buf.[i])
            i <- i + 1
        ew

    let boolW: Writer<bool, BooleanF> = {
        new Writer<bool, BooleanF>() with 
            member w.Format = Formats.booleanF
            member w.Bind(s:Sink) = fun b -> boolF b s
    }
    let byteW: Writer<byte, ByteF> = {
        new Writer<byte, ByteF>() with
            member w.Format = Formats.byteF           
            member w.Bind(s:Sink) = fun b -> byteF b s
    }
    let shortW: Writer<int16, ShortF> = {
        new Writer<int16, ShortF>() with
            member w.Format = Formats.shortF            
            member w.Bind(s:Sink) = fun i -> shortF i s
    }
    let intW: Writer<int, IntF> = {
        new Writer<int, IntF>() with
            member w.Format = Formats.intF                       
            member w.Bind(s:Sink) = fun i -> intF i s
    }
    let longW: Writer<int64, LongF> = {
        new Writer<int64, LongF>() with
            member w.Format = Formats.longF                       
            member w.Bind(s:Sink) = fun l -> longF l s
    }
    let unitW: Writer<unit, UnitF> = {
        new Writer<unit, UnitF>() with
            member w.Format = Formats.unitF                       
            member w.Bind(s:Sink) = fun _ -> ew
    }
    let stringW: Writer<string, StringF> = {
        new Writer<string, StringF>() with
            member w.Format = Formats.stringF           
            member w.Bind(s:Sink) = fun str -> stringF str s
    }
    let doubleW: Writer<double, DoubleF> = {
        new Writer<double, DoubleF>() with 
            member w.Format = Formats.doubleF           
            member w.Bind(s:Sink) = fun d -> doubleF d s
    }
    let singleW: Writer<single, FloatF> = {
        new Writer<single, FloatF>() with
            member w.Format = Formats.floatF           
            member w.Bind(s:Sink) = fun d -> singleF d s
    }
    let streamW(w:Writer<'a,'f1>) = {
        new Writer<seq<'a>, StreamF<'f1>>() with 
            member this.Format = Formats.streamF(w.Format)
            member this.Bind(s:Sink) =
                let bindA = w.Bind(s)
                (fun (xs:seq<'a>) ->
                    xs |> Seq.iter (fun x -> s.WriteBit(true); bindA(x); ())
                    s.WriteBit(false)
                    ew
                )
    }

    let unfoldW(w:Writer<'a,'f1>)(f: 's -> Option<'a * 's>) = {
        new Writer<'s, StreamF<'f1>>() with 
            member this.Format = Formats.streamF(w.Format)
            member this.Bind(o:Sink) =
                let bindA = w.Bind(o)
                (fun (s:'s) ->
                    let mutable cur = s
                    let mutable cont = true
                    while cont do
                        match f cur with
                        | Some (h,t) -> 
                            o.WriteBit(true)
                            bindA(h)
                            cur <- t
                        | None -> cont <- false
                    o.WriteBit(false)
                    ew
                )
    }
    
    let repeatW<'a, 'f>(w:Writer<'a, 'f>): Writer<List<'a>, RepeatF<'f>> = {
        new Writer<List<'a>, RepeatF<'f>>() with 
            member this.Format = Formats.repeatF(w.Format)
            member this.Bind(o:Sink) =
                let bindA = w.Bind(o)
                (fun (xs:List<'a>) ->
                    intF xs.Length o
                    xs |> List.iter (fun x -> bindA x; ())
                    ew
                )
    }

    type Out<'a, 'b> = Writer<'a, 'b>
    type EfW<'a> = EffectW<'a>
      
    let p2W(wa: Out<'a,'f1>, wb: Out<'b,'f2>)(f: (('a * 'b) -> EfW<P2<'f1,'f2>>) -> 'r -> EfW<P2<'f1,'f2>>) = {
        new Out<'r,P2<'f1, 'f2>>() with 
            member this.Format = Formats.p2F(wa.Format, wb.Format)
            member this.Bind(s: Sink) =
                let (bindA,bindB) = (wa.Bind(s),wb.Bind(s))
                f((fun (a,b) -> bindA(a); bindB(b); ew))
    }
    let p3W(wa: Out<'a,'f1>, wb: Out<'b,'f2>, wc: Out<'c,'f3>)
           (f: (('a * 'b * 'c) -> EfW<P3<'f1,'f2,'f3>>) -> 'r -> EfW<P3<'f1,'f2,'f3>>) = {
        new Out<'r,P3<'f1,'f2,'f3>>() with
            member this.Format = Formats.p3F(wa.Format, wb.Format, wc.Format) 
            member this.Bind(s: Sink) =
                let (bindA,bindB,bindC) = (wa.Bind(s),wb.Bind(s),wc.Bind(s))
                f((fun (a,b,c) -> bindA(a); bindB(b); bindC(c); ew))
    }
    let p4W(wa: Out<'a,'f1>, wb: Out<'b,'f2>, wc: Out<'c,'f3>, wd: Out<'d,'f4>)
           (f: (('a * 'b * 'c * 'd) -> EfW<P4<'f1,'f2,'f3,'f4>>) -> 'r -> EfW<P4<'f1,'f2,'f3,'f4>>) = {
        new Out<'r,P4<'f1,'f2,'f3,'f4>>() with 
            member this.Format = Formats.p4F(wa.Format, wb.Format, wc.Format, wd.Format)
            member r.Bind(s: Sink) =
                let (bindA,bindB,bindC,bindD) = (wa.Bind(s),wb.Bind(s),wc.Bind(s),wd.Bind(s))
                f((fun (a,b,c,d) -> bindA(a); bindB(b); bindC(c); bindD(d); ew))
    }
    let p5W(wa: Out<'a,'f1>, wb: Out<'b,'f2>, wc: Out<'c,'f3>, wd: Out<'d,'f4>, we: Out<'e,'f5>)
           (f: (('a * 'b * 'c * 'd * 'e) -> EfW<P5<'f1,'f2,'f3,'f4,'f5>>) -> 'r -> EfW<P5<'f1,'f2,'f3,'f4,'f5>>) = {
        new Out<'r,P5<'f1,'f2,'f3,'f4,'f5>>() with 
            member this.Format = Formats.p5F(wa.Format, wb.Format, wc.Format, wd.Format, we.Format)            
            member r.Bind(s: Sink) =
                let (bindA,bindB,bindC,bindD,bindE) = (wa.Bind(s),wb.Bind(s),wc.Bind(s),wd.Bind(s),we.Bind(s))
                f((fun (a,b,c,d,e) -> bindA(a); bindB(b); bindC(c); bindD(d); bindE(e); ew))
    }
    let p6W(wa: Out<'a,'f1>, wb: Out<'b,'f2>, wc: Out<'c,'f3>, wd: Out<'d,'f4>, we: Out<'e,'f5>, wf: Out<'f,'f6>)
           (f: (('a * 'b * 'c * 'd * 'e * 'f) -> EfW<P6<'f1,'f2,'f3,'f4,'f5,'f6>>) -> 'r -> EfW<P6<'f1,'f2,'f3,'f4,'f5,'f6>>) = {
        new Out<'r,P6<'f1,'f2,'f3,'f4,'f5,'f6>>() with 
            member this.Format = Formats.p6F(wa.Format, wb.Format, wc.Format, wd.Format, we.Format, wf.Format)
            member r.Bind(s: Sink) =
                let (bindA,bindB,bindC,bindD,bindE,bindF) = (wa.Bind(s),wb.Bind(s),wc.Bind(s),wd.Bind(s),we.Bind(s),wf.Bind(s))
                f((fun (a,b,c,d,e,f) -> bindA(a); bindB(b); bindC(c); bindD(d); bindE(e); bindF(f); ew))
    }
    let tuple2W(wa,wb) = p2W(wa,wb)(<|)
    let tuple3W(wa,wb,wc) = p3W(wa,wb,wc)(<|)
    let tuple4W(wa,wb,wc,wd) = p4W(wa,wb,wc,wd)(<|)
    let tuple5W(wa,wb,wc,wd,we) = p5W(wa,wb,wc,wd,we)(<|)
    let tuple6W(wa,wb,wc,wd,we,wf) = p6W(wa,wb,wc,wd,we,wf)(<|)

    let fixW(f: Writer<'a,'f> -> Writer<'a,'f>) = new FixW<'a, 'f>(f)

(*** AUTO GENERATED CODE BELOW ***)

    (* 2-way sum writer *)
    let s2W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>)
            (f: (('a0 -> EffectW<S2<'f0,'f1>>) * ('a1 -> EffectW<S2<'f0,'f1>>)) -> 'r -> EffectW<S2<'f0,'f1>>) = {
        new Writer<'r,S2<'f0,'f1>>() with
            member w.Format = Formats.s2F(w0.Format, w1.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew))
    }

    (* 3-way sum writer *)
    let s3W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>)
            (f: (('a0 -> EffectW<S3<'f0,'f1,'f2>>) * ('a1 -> EffectW<S3<'f0,'f1,'f2>>) * ('a2 -> EffectW<S3<'f0,'f1,'f2>>)) -> 'r -> EffectW<S3<'f0,'f1,'f2>>) = {
        new Writer<'r,S3<'f0,'f1,'f2>>() with
            member w.Format = Formats.s3F(w0.Format, w1.Format, w2.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew))
    }

    (* 4-way sum writer *)
    let s4W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>)
            (f: (('a0 -> EffectW<S4<'f0,'f1,'f2,'f3>>) * ('a1 -> EffectW<S4<'f0,'f1,'f2,'f3>>) * ('a2 -> EffectW<S4<'f0,'f1,'f2,'f3>>) * ('a3 -> EffectW<S4<'f0,'f1,'f2,'f3>>)) -> 'r -> EffectW<S4<'f0,'f1,'f2,'f3>>) = {
        new Writer<'r,S4<'f0,'f1,'f2,'f3>>() with
            member w.Format = Formats.s4F(w0.Format, w1.Format, w2.Format, w3.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew))
    }

    (* 5-way sum writer *)
    let s5W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>)
            (f: (('a0 -> EffectW<S5<'f0,'f1,'f2,'f3,'f4>>) * ('a1 -> EffectW<S5<'f0,'f1,'f2,'f3,'f4>>) * ('a2 -> EffectW<S5<'f0,'f1,'f2,'f3,'f4>>) * ('a3 -> EffectW<S5<'f0,'f1,'f2,'f3,'f4>>) * ('a4 -> EffectW<S5<'f0,'f1,'f2,'f3,'f4>>)) -> 'r -> EffectW<S5<'f0,'f1,'f2,'f3,'f4>>) = {
        new Writer<'r,S5<'f0,'f1,'f2,'f3,'f4>>() with
            member w.Format = Formats.s5F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew))
    }

    (* 6-way sum writer *)
    let s6W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>)
            (f: (('a0 -> EffectW<S6<'f0,'f1,'f2,'f3,'f4,'f5>>) * ('a1 -> EffectW<S6<'f0,'f1,'f2,'f3,'f4,'f5>>) * ('a2 -> EffectW<S6<'f0,'f1,'f2,'f3,'f4,'f5>>) * ('a3 -> EffectW<S6<'f0,'f1,'f2,'f3,'f4,'f5>>) * ('a4 -> EffectW<S6<'f0,'f1,'f2,'f3,'f4,'f5>>) * ('a5 -> EffectW<S6<'f0,'f1,'f2,'f3,'f4,'f5>>)) -> 'r -> EffectW<S6<'f0,'f1,'f2,'f3,'f4,'f5>>) = {
        new Writer<'r,S6<'f0,'f1,'f2,'f3,'f4,'f5>>() with
            member w.Format = Formats.s6F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew))
    }

    (* 7-way sum writer *)
    let s7W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>)
            (f: (('a0 -> EffectW<S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>) * ('a1 -> EffectW<S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>) * ('a2 -> EffectW<S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>) * ('a3 -> EffectW<S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>) * ('a4 -> EffectW<S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>) * ('a5 -> EffectW<S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>) * ('a6 -> EffectW<S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>)) -> 'r -> EffectW<S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>) = {
        new Writer<'r,S7<'f0,'f1,'f2,'f3,'f4,'f5,'f6>>() with
            member w.Format = Formats.s7F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew))
    }

    (* 8-way sum writer *)
    let s8W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>)
            (f: (('a0 -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>) * ('a1 -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>) * ('a2 -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>) * ('a3 -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>) * ('a4 -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>) * ('a5 -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>) * ('a6 -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>) * ('a7 -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>)) -> 'r -> EffectW<S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>) = {
        new Writer<'r,S8<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7>>() with
            member w.Format = Formats.s8F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew))
    }

    (* 9-way sum writer *)
    let s9W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>)
            (f: (('a0 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) * ('a1 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) * ('a2 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) * ('a3 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) * ('a4 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) * ('a5 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) * ('a6 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) * ('a7 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) * ('a8 -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>)) -> 'r -> EffectW<S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>) = {
        new Writer<'r,S9<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8>>() with
            member w.Format = Formats.s9F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew))
    }

    (* 10-way sum writer *)
    let s10W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>)
            (f: (('a0 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a1 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a2 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a3 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a4 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a5 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a6 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a7 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a8 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) * ('a9 -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>)) -> 'r -> EffectW<S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>) = {
        new Writer<'r,S10<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9>>() with
            member w.Format = Formats.s10F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew))
    }

    (* 11-way sum writer *)
    let s11W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>)
            (f: (('a0 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a1 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a2 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a3 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a4 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a5 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a6 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a7 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a8 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a9 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) * ('a10 -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>)) -> 'r -> EffectW<S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>) = {
        new Writer<'r,S11<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10>>() with
            member w.Format = Formats.s11F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew))
    }

    (* 12-way sum writer *)
    let s12W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>)
            (f: (('a0 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a1 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a2 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a3 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a4 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a5 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a6 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a7 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a8 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a9 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a10 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) * ('a11 -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>)) -> 'r -> EffectW<S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>) = {
        new Writer<'r,S12<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11>>() with
            member w.Format = Formats.s12F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew))
    }

    (* 13-way sum writer *)
    let s13W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>)
            (f: (('a0 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a1 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a2 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a3 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a4 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a5 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a6 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a7 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a8 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a9 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a10 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a11 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) * ('a12 -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>)) -> 'r -> EffectW<S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>) = {
        new Writer<'r,S13<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12>>() with
            member w.Format = Formats.s13F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew))
    }

    (* 14-way sum writer *)
    let s14W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>)
            (f: (('a0 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a1 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a2 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a3 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a4 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a5 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a6 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a7 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a8 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a9 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a10 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a11 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a12 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) * ('a13 -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>)) -> 'r -> EffectW<S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>) = {
        new Writer<'r,S14<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13>>() with
            member w.Format = Formats.s14F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew))
    }

    (* 15-way sum writer *)
    let s15W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>, w14: Writer<'a14,'f14>)
            (f: (('a0 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a1 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a2 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a3 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a4 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a5 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a6 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a7 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a8 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a9 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a10 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a11 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a12 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a13 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) * ('a14 -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>)) -> 'r -> EffectW<S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>) = {
        new Writer<'r,S15<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14>>() with
            member w.Format = Formats.s15F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format, w14.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                let w14Bound = w14.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew), (fun a -> s.WriteByte(14uy); w14Bound(a); ew))
    }

    (* 16-way sum writer *)
    let s16W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>, w14: Writer<'a14,'f14>, w15: Writer<'a15,'f15>)
            (f: (('a0 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a1 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a2 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a3 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a4 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a5 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a6 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a7 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a8 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a9 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a10 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a11 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a12 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a13 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a14 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) * ('a15 -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>)) -> 'r -> EffectW<S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>) = {
        new Writer<'r,S16<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15>>() with
            member w.Format = Formats.s16F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format, w14.Format, w15.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                let w14Bound = w14.Bind(s)
                let w15Bound = w15.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew), (fun a -> s.WriteByte(14uy); w14Bound(a); ew), (fun a -> s.WriteByte(15uy); w15Bound(a); ew))
    }

    (* 17-way sum writer *)
    let s17W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>, w14: Writer<'a14,'f14>, w15: Writer<'a15,'f15>, w16: Writer<'a16,'f16>)
            (f: (('a0 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a1 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a2 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a3 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a4 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a5 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a6 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a7 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a8 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a9 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a10 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a11 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a12 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a13 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a14 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a15 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) * ('a16 -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>)) -> 'r -> EffectW<S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>) = {
        new Writer<'r,S17<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16>>() with
            member w.Format = Formats.s17F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format, w14.Format, w15.Format, w16.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                let w14Bound = w14.Bind(s)
                let w15Bound = w15.Bind(s)
                let w16Bound = w16.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew), (fun a -> s.WriteByte(14uy); w14Bound(a); ew), (fun a -> s.WriteByte(15uy); w15Bound(a); ew), (fun a -> s.WriteByte(16uy); w16Bound(a); ew))
    }

    (* 18-way sum writer *)
    let s18W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>, w14: Writer<'a14,'f14>, w15: Writer<'a15,'f15>, w16: Writer<'a16,'f16>, w17: Writer<'a17,'f17>)
            (f: (('a0 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a1 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a2 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a3 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a4 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a5 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a6 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a7 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a8 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a9 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a10 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a11 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a12 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a13 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a14 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a15 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a16 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) * ('a17 -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>)) -> 'r -> EffectW<S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>) = {
        new Writer<'r,S18<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17>>() with
            member w.Format = Formats.s18F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format, w14.Format, w15.Format, w16.Format, w17.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                let w14Bound = w14.Bind(s)
                let w15Bound = w15.Bind(s)
                let w16Bound = w16.Bind(s)
                let w17Bound = w17.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew), (fun a -> s.WriteByte(14uy); w14Bound(a); ew), (fun a -> s.WriteByte(15uy); w15Bound(a); ew), (fun a -> s.WriteByte(16uy); w16Bound(a); ew), (fun a -> s.WriteByte(17uy); w17Bound(a); ew))
    }

    (* 19-way sum writer *)
    let s19W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>, w14: Writer<'a14,'f14>, w15: Writer<'a15,'f15>, w16: Writer<'a16,'f16>, w17: Writer<'a17,'f17>, w18: Writer<'a18,'f18>)
            (f: (('a0 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a1 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a2 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a3 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a4 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a5 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a6 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a7 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a8 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a9 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a10 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a11 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a12 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a13 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a14 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a15 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a16 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a17 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) * ('a18 -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>)) -> 'r -> EffectW<S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>) = {
        new Writer<'r,S19<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18>>() with
            member w.Format = Formats.s19F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format, w14.Format, w15.Format, w16.Format, w17.Format, w18.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                let w14Bound = w14.Bind(s)
                let w15Bound = w15.Bind(s)
                let w16Bound = w16.Bind(s)
                let w17Bound = w17.Bind(s)
                let w18Bound = w18.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew), (fun a -> s.WriteByte(14uy); w14Bound(a); ew), (fun a -> s.WriteByte(15uy); w15Bound(a); ew), (fun a -> s.WriteByte(16uy); w16Bound(a); ew), (fun a -> s.WriteByte(17uy); w17Bound(a); ew), (fun a -> s.WriteByte(18uy); w18Bound(a); ew))
    }

    (* 20-way sum writer *)
    let s20W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>, w14: Writer<'a14,'f14>, w15: Writer<'a15,'f15>, w16: Writer<'a16,'f16>, w17: Writer<'a17,'f17>, w18: Writer<'a18,'f18>, w19: Writer<'a19,'f19>)
            (f: (('a0 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a1 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a2 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a3 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a4 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a5 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a6 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a7 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a8 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a9 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a10 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a11 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a12 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a13 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a14 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a15 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a16 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a17 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a18 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) * ('a19 -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>)) -> 'r -> EffectW<S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>) = {
        new Writer<'r,S20<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19>>() with
            member w.Format = Formats.s20F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format, w14.Format, w15.Format, w16.Format, w17.Format, w18.Format, w19.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                let w14Bound = w14.Bind(s)
                let w15Bound = w15.Bind(s)
                let w16Bound = w16.Bind(s)
                let w17Bound = w17.Bind(s)
                let w18Bound = w18.Bind(s)
                let w19Bound = w19.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew), (fun a -> s.WriteByte(14uy); w14Bound(a); ew), (fun a -> s.WriteByte(15uy); w15Bound(a); ew), (fun a -> s.WriteByte(16uy); w16Bound(a); ew), (fun a -> s.WriteByte(17uy); w17Bound(a); ew), (fun a -> s.WriteByte(18uy); w18Bound(a); ew), (fun a -> s.WriteByte(19uy); w19Bound(a); ew))
    }

    (* 21-way sum writer *)
    let s21W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>, w14: Writer<'a14,'f14>, w15: Writer<'a15,'f15>, w16: Writer<'a16,'f16>, w17: Writer<'a17,'f17>, w18: Writer<'a18,'f18>, w19: Writer<'a19,'f19>, w20: Writer<'a20,'f20>)
            (f: (('a0 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a1 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a2 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a3 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a4 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a5 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a6 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a7 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a8 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a9 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a10 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a11 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a12 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a13 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a14 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a15 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a16 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a17 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a18 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a19 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) * ('a20 -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>)) -> 'r -> EffectW<S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>) = {
        new Writer<'r,S21<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20>>() with
            member w.Format = Formats.s21F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format, w14.Format, w15.Format, w16.Format, w17.Format, w18.Format, w19.Format, w20.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                let w14Bound = w14.Bind(s)
                let w15Bound = w15.Bind(s)
                let w16Bound = w16.Bind(s)
                let w17Bound = w17.Bind(s)
                let w18Bound = w18.Bind(s)
                let w19Bound = w19.Bind(s)
                let w20Bound = w20.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew), (fun a -> s.WriteByte(14uy); w14Bound(a); ew), (fun a -> s.WriteByte(15uy); w15Bound(a); ew), (fun a -> s.WriteByte(16uy); w16Bound(a); ew), (fun a -> s.WriteByte(17uy); w17Bound(a); ew), (fun a -> s.WriteByte(18uy); w18Bound(a); ew), (fun a -> s.WriteByte(19uy); w19Bound(a); ew), (fun a -> s.WriteByte(20uy); w20Bound(a); ew))
    }

    (* 22-way sum writer *)
    let s22W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>, w2: Writer<'a2,'f2>, w3: Writer<'a3,'f3>, w4: Writer<'a4,'f4>, w5: Writer<'a5,'f5>, w6: Writer<'a6,'f6>, w7: Writer<'a7,'f7>, w8: Writer<'a8,'f8>, w9: Writer<'a9,'f9>, w10: Writer<'a10,'f10>, w11: Writer<'a11,'f11>, w12: Writer<'a12,'f12>, w13: Writer<'a13,'f13>, w14: Writer<'a14,'f14>, w15: Writer<'a15,'f15>, w16: Writer<'a16,'f16>, w17: Writer<'a17,'f17>, w18: Writer<'a18,'f18>, w19: Writer<'a19,'f19>, w20: Writer<'a20,'f20>, w21: Writer<'a21,'f21>)
            (f: (('a0 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a1 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a2 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a3 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a4 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a5 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a6 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a7 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a8 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a9 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a10 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a11 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a12 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a13 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a14 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a15 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a16 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a17 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a18 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a19 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a20 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) * ('a21 -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>)) -> 'r -> EffectW<S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>) = {
        new Writer<'r,S22<'f0,'f1,'f2,'f3,'f4,'f5,'f6,'f7,'f8,'f9,'f10,'f11,'f12,'f13,'f14,'f15,'f16,'f17,'f18,'f19,'f20,'f21>>() with
            member w.Format = Formats.s22F(w0.Format, w1.Format, w2.Format, w3.Format, w4.Format, w5.Format, w6.Format, w7.Format, w8.Format, w9.Format, w10.Format, w11.Format, w12.Format, w13.Format, w14.Format, w15.Format, w16.Format, w17.Format, w18.Format, w19.Format, w20.Format, w21.Format)
            member w.Bind(s: Sink) =
                let w0Bound = w0.Bind(s)
                let w1Bound = w1.Bind(s)
                let w2Bound = w2.Bind(s)
                let w3Bound = w3.Bind(s)
                let w4Bound = w4.Bind(s)
                let w5Bound = w5.Bind(s)
                let w6Bound = w6.Bind(s)
                let w7Bound = w7.Bind(s)
                let w8Bound = w8.Bind(s)
                let w9Bound = w9.Bind(s)
                let w10Bound = w10.Bind(s)
                let w11Bound = w11.Bind(s)
                let w12Bound = w12.Bind(s)
                let w13Bound = w13.Bind(s)
                let w14Bound = w14.Bind(s)
                let w15Bound = w15.Bind(s)
                let w16Bound = w16.Bind(s)
                let w17Bound = w17.Bind(s)
                let w18Bound = w18.Bind(s)
                let w19Bound = w19.Bind(s)
                let w20Bound = w20.Bind(s)
                let w21Bound = w21.Bind(s)
                f((fun a -> s.WriteByte(0uy); w0Bound(a); ew), (fun a -> s.WriteByte(1uy); w1Bound(a); ew), (fun a -> s.WriteByte(2uy); w2Bound(a); ew), (fun a -> s.WriteByte(3uy); w3Bound(a); ew), (fun a -> s.WriteByte(4uy); w4Bound(a); ew), (fun a -> s.WriteByte(5uy); w5Bound(a); ew), (fun a -> s.WriteByte(6uy); w6Bound(a); ew), (fun a -> s.WriteByte(7uy); w7Bound(a); ew), (fun a -> s.WriteByte(8uy); w8Bound(a); ew), (fun a -> s.WriteByte(9uy); w9Bound(a); ew), (fun a -> s.WriteByte(10uy); w10Bound(a); ew), (fun a -> s.WriteByte(11uy); w11Bound(a); ew), (fun a -> s.WriteByte(12uy); w12Bound(a); ew), (fun a -> s.WriteByte(13uy); w13Bound(a); ew), (fun a -> s.WriteByte(14uy); w14Bound(a); ew), (fun a -> s.WriteByte(15uy); w15Bound(a); ew), (fun a -> s.WriteByte(16uy); w16Bound(a); ew), (fun a -> s.WriteByte(17uy); w17Bound(a); ew), (fun a -> s.WriteByte(18uy); w18Bound(a); ew), (fun a -> s.WriteByte(19uy); w19Bound(a); ew), (fun a -> s.WriteByte(20uy); w20Bound(a); ew), (fun a -> s.WriteByte(21uy); w21Bound(a); ew))
    }

(*** AUTO GENERATED CODE ABOVE ***)

    let choiceW(wa,wb): Writer<Choice<'a,'b>,S2<'f1,'f2>> = s2W(wa,wb)(fold)
    let optionW(wa) = s2W(unitW,wa)(fun (none,some) -> (fun o -> match o with
        | None -> none(())
        | Some a -> some(a)
    ))