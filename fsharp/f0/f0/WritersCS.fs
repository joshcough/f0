namespace f0

open Effects
open Writers
open System.Collections.Generic
open System.Linq;

module WritersCS =

    // TODO: should we use the existing repeatW here? 
    // i think so, but doing so means a conversion and allocation.
    let repeatW<'a, 'f>(w:Writer<'a, 'f>): Writer<List<'a>, RepeatF<'f>> = {
        new Writer<List<'a>, RepeatF<'f>>() with
            member this.Format = Formats.repeatF(w.Format)
            member this.Bind(o:Sink) =
                let bindA = w.Bind(o)
                (fun xs ->
                    intF xs.Count o
                    let mutable e = xs.GetEnumerator()
                    while e.MoveNext() do
                        bindA(e.Current)
                    ew
                )
    }

    //let p2W(wa: Out<'a,'f1>, wb: Out<'b,'f2>, 
    //        func: System.Func<System.Func<'a,'b, EfW<P2<'f1,'f2>>>, System.Func<'r, EfW<P2<'f1,'f2>>>>): Writer<'r,P2<'f1,'f2>> =
    //    Writers.p2W(wa, wb)(fun ab -> fun r -> 
    //        func.Invoke(new System.Func<'a,'b, EfW<P2<'f1,'f2>>>(FuncConvert.FuncFromTupled(ab))).Invoke(r))
    //
    //let p2W(wa: Out<'a,'f1>, wb: Out<'b,'f2>, 
    //        func: System.Func<System.Func<'a,'b, EfW<P2<'f1,'f2>>>, 'r, EfW<P2<'f1,'f2>>>): Writer<'r,P2<'f1,'f2>> =
    //    Writers.p2W(wa, wb)(fun ab -> fun r -> func.Invoke(new System.Func<'a,'b, EfW<P2<'f1,'f2>>>(FuncConvert.FuncFromTupled(ab)), r))
    //
    //let p2W(wa: Out<'a,'f1>, wb: Out<'b,'f2>, func: System.Func<('a * 'b) -> EfW<P2<'f1,'f2>>, 'r, EfW<P2<'f1,'f2>>>): Writer<'r,P2<'f1,'f2>> =
    //    Writers.p2W(wa, wb)(fun ab -> fun r -> func.Invoke(ab, r))

    //type P2WTry1<'a,'f1,'b,'f2> = 
    //    | P2W of Out<'a,'f1> * Out<'b,'f2>
    //    member self.Invoke<'r>(func: System.Func<System.Func<'a,'b, EfW<P2<'f1,'f2>>>, System.Func<'r, EfW<P2<'f1,'f2>>>>): Writer<'r,P2<'f1,'f2>> = 
    //        match self with | P2W(wa,wb) -> Writers.p2W(wa, wb)(fun ab -> fun r -> 
    //            func.Invoke(new System.Func<'a,'b, EfW<P2<'f1,'f2>>>(FuncConvert.FuncFromTupled(ab))).Invoke(r))
    //

    type P2W<'a,'f1,'b,'f2> = 
        | P2W of Out<'a,'f1> * Out<'b,'f2>
        member self.Invoke<'r>(func: System.Func<System.Func<'a,'b, EfW<P2<'f1,'f2>>>, 'r, EfW<P2<'f1,'f2>>>): Writer<'r,P2<'f1,'f2>> = 
            match self with | P2W(wa,wb) -> Writers.p2W(wa, wb)(fun ab -> fun r -> 
                func.Invoke(new System.Func<'a,'b, EfW<P2<'f1,'f2>>>(FuncConvert.FuncFromTupled(ab)), r))

    let p2W(wa: Out<'a,'f1>, wb: Out<'b,'f2>) = P2W(wa, wb)

////let p2W(wa: Writer<'a,'f1>, wb: Writer<'b,'f2>)(f: (('a * 'b) -> EfW<P2<'f1,'f2>>) -> 'r -> EfW<P2<'f1,'f2>>) = {
////        new Writer<'r,P2<'f1, 'f2>>() with 
//
////let s2W(w0: Writer<'a0,'f0>, w1: Writer<'a1,'f1>) (f: (('a0 -> EffectW<S2<'f0,'f1>>) * ('a1 -> EffectW<S2<'f0,'f1>>)) -> 'r -> EffectW<S2<'f0,'f1>>) = {
////        new Writer<'r,S2<'f0,'f1>>() with
//
////    let choiceW(wa,wb): Writer<Choice<'a,'b>,S2<'f1,'f2>> = s2W(wa,wb)(fold)
////    let optionW(wa) = s2W(unitW,wa)(fun (none,some) -> (fun o -> match o with
////        | None -> none(())
////        | Some a -> some(a)
////    ))
//
//
//    type S2W<'a,'f1,'b,'f2> = 
//        | S2W of Out<'a,'f1> * Out<'b,'f2>
//        member self.Invoke<'r>(func: System.Func<System.Func<'a,'b, EfW<P2<'f1,'f2>>>, 'r, EfW<P2<'f1,'f2>>>): Writer<'r,P2<'f1,'f2>> = 
//            match self with | S2W(wa,wb) -> Writers.s2W(wa, wb)(fun ab -> fun r -> 
//                func.Invoke(new System.Func<'a,'b, EfW<P2<'f1,'f2>>>(FuncConvert.FuncFromTupled(ab)), r))
//
//    let s2W(wa: Out<'a,'f1>, wb: Out<'b,'f2>) = S2W(wa, wb)