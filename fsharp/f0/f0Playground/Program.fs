open FsCheck

open f0
open f0.Readers
open f0.Writers
open f0.Formats
open f0.Sinks
open f0.Sources

let roundTrip<'a, 'f>(a:'a, w:Writer<'a, 'f>, r:Reader<'a, 'f>) : 'a =
    //printfn "in = %A" a
    let ms = new System.IO.MemoryStream()
    let sink = Sinks.ToOutputStream ms
    let _ = w.Bind sink a
    sink.Finished()
    let out = r.Bind(Sources.FromByteArray(ms.ToArray())).Get
    //printfn "out = %A" out
    out

let checks() = 
    FsCheck.Check.Quick (fun (b1:byte, b2:byte) -> 
        let s = Readers.bytesToInt16 b1 b2
        roundTrip(s, shortW, shortR) = s
        )
    FsCheck.Check.Quick (fun (i:int)  -> roundTrip(i, intW, intR)   = i)
    FsCheck.Check.Quick (fun (b:bool) -> roundTrip(b, boolW, boolR) = b)
    FsCheck.Check.Quick (fun (b:byte) -> roundTrip(b, byteW, byteR) = b)
    FsCheck.Check.Quick (fun (s:string) -> roundTrip(s, stringW, stringR) = s)
    FsCheck.Check.Quick (fun (d:double) -> 
        let r = roundTrip(d, doubleW, doubleR)
        if System.Double.IsNaN d then System.Double.IsNaN r else r = d
    )
    // TODO: figure out how to make this work. fscheck doesnt generate single/float32.
    //FsCheck.Check.Quick (fun (d:double) -> 
    //    roundTrip(float32 d, singleW, singleR) = float32 d)
    // fscheck doesnt have an Arb instance for int64, so i do this trick
    FsCheck.Check.Quick (fun (d:double) -> 
        let i: int64 = System.BitConverter.DoubleToInt64Bits(d)
        roundTrip(i, longW, longR) = i)
    //FsCheck.Check.Quick (fun (l:int64) -> roundTrip(l, longW, longR) = l)
    FsCheck.Check.Quick (fun (bs:List<bool>) -> roundTrip(bs, repeatW(boolW), listR(boolR)) = bs)
    FsCheck.Check.Quick (fun (bs:List<string>) -> roundTrip(bs, repeatW(stringW), listR(stringR)) = bs)
    FsCheck.Check.Quick (fun (bs:List<string>) -> 
        let bss = bs |> List.toSeq
        roundTrip(bss, streamW(stringW), streamR(stringR).Map(List.toSeq)) = bss)

    FsCheck.Check.Quick (fun (bs:List<List<string>>) -> 
        let bss = List.map List.toSeq bs |> List.toSeq
        roundTrip(bss, streamW(streamW(stringW)), streamR(streamR(stringR).Map(List.toSeq)).Map(List.toSeq)) = bss)

    FsCheck.Check.Quick (fun (s1:string, s2:string) -> 
        roundTrip((s1, s2), tuple2W(stringW,stringW), tuple2R(stringR,stringR)) = (s1, s2)
    )
    FsCheck.Check.Quick (fun (s1:string, s2:string, s3:string) -> 
        roundTrip((s1, s2, s3), tuple3W(stringW,stringW,stringW), tuple3R(stringR,stringR,stringR)) = (s1, s2, s3)
    )
    FsCheck.Check.Quick (fun (s:string, i:int) -> 
        roundTrip((s, i), tuple2W(stringW,intW), tuple2R(stringR,intR)) = (s, i)
    )
    FsCheck.Check.Quick (fun (s:string, i:int, b:byte) -> 
        roundTrip((s, i, b), tuple3W(stringW,intW, byteW), tuple3R(stringR,intR,byteR)) = (s, i, b)
    )
    FsCheck.Check.Quick (fun (s1:string, s2:string) -> 
        roundTrip((s1, s2), tuple2W(stringW,stringW), tuple2R(stringR,stringR)) = (s1, s2)
    )
    FsCheck.Check.Quick (fun (c1:Choice<string,int>, c2: Choice<int,string>) -> 
        roundTrip((c1, c2), 
            tuple2W(choiceW(stringW,intW), choiceW(intW,stringW)), 
            tuple2R(choiceR(stringR,intR), choiceR(intR,stringR))) = (c1, c2)
    )
    FsCheck.Check.Quick (fun (l:List<(Choice<string,int> * Choice<int,string>)>) -> 
        roundTrip(l, 
            repeatW(tuple2W(choiceW(stringW,intW), choiceW(intW,stringW))), 
            listR(tuple2R(choiceR(stringR,intR), choiceR(intR,stringR)))) = l
    )
    FsCheck.Check.Quick (fun (l:List<(Choice<string,int> * Choice<int,string>)>) -> 
        let s = l |> List.toSeq
        roundTrip(s, 
            streamW(tuple2W(choiceW(stringW,intW), choiceW(intW,stringW))), 
            streamR(tuple2R(choiceR(stringR,intR), choiceR(intR,stringR))).Map(List.toSeq)) = s
    )
    ()

let simpleWrites(sink:Sink) =
    let _ = Writers.intW.Bind sink 7
    let _ = Writers.boolW.Bind sink true
    let _ = Writers.boolW.Bind sink false
    let _ = Writers.intW.Bind sink 42
    let _ = Writers.longW.Bind sink 9223372036854775807L
    let _ = Writers.longW.Bind sink -9223372036854775808L
    let _ = Writers.longW.Bind sink 0L
    let _ = Writers.stringW.Bind sink "hello, world"
    let _ = Writers.doubleW.Bind sink 3.1415
    let _ = Writers.singleW.Bind sink 3.1415f
    let _ = Writers.repeatW(intW).Bind sink [1; 2; 3; 4; 5]
    let _ = tuple2W(intW, intW).Bind sink (1, 2)
    let _ = tuple3W(intW, intW, intW).Bind sink (1, 2, 3)
    let _ = Writers.streamW(intW).Bind sink (seq {1 .. 5})
    ()

let simpleReads(source:Source) =
    printfn "%i" (Readers.intR.Bind(source).Get)
    printfn "%b" (Readers.boolR.Bind(source).Get)
    printfn "%b" (Readers.boolR.Bind(source).Get)
    printfn "%i" (Readers.intR.Bind(source).Get)
    printfn "%i" (Readers.longR.Bind(source).Get)
    printfn "%i" (Readers.longR.Bind(source).Get)
    printfn "%i" (Readers.longR.Bind(source).Get)
    printfn "%s" (Readers.stringR.Bind(source).Get)
    printfn "%s" (sprintf "%.4f" (Readers.doubleR.Bind(source).Get))
    printfn "%s" (sprintf "%.4f" (Readers.singleR.Bind(source).Get))
    printfn "%A" (Readers.listR(intR).Bind(source).Get)
    printfn "%A" (Readers.tuple2R(intR, intR).Bind(source).Get)
    printfn "%A" (Readers.tuple3R(intR, intR, intR).Bind(source).Get)
    printfn "%A" (Readers.streamR(intR).Bind(source).Get)

let simpleReadsAndWrites(sink:Sink, sourceF: unit -> Source) =
    sink.Using(fun s -> simpleWrites(s))
    simpleReads(sourceF())
    
let simpleReadsAndWritesToByteArray() =
    let ms = new System.IO.MemoryStream()
    let sink = Sinks.ToOutputStream ms
    simpleReadsAndWrites(sink, (fun x -> Sources.FromByteArray(ms.ToArray())))

let simpleReadsAndWritesToFile() =
    //let filename = System.IO.Path.GetTempFileName()
    // writes data-from-fsharp.dat in the f0 root directory,
    // where it can be consumed by scala for integration testing.
    // much more on this to come soon.
    let filename = "../../../../../test/data-from-fsharp.dat"
    let stream = System.IO.File.Create(filename)
    stream.Close()
    let sink = Sinks.ToFile(filename)
    printfn "sinking to %s" stream.Name 
    simpleReadsAndWrites(sink, (fun x -> Sources.FromFile(filename)))


let formatTesting() =
    // here ive explicitly put in the type param
    let rs = new RepeatF<StringF>(new StringF())
    printfn "%A" rs

    // the next line doesnt compile. type param is not inferred on constructors.
    // let rs2 = new RepeatF(new StringF())
    
    // the next line does work. type param properly inferred on function calls
    let rs3 = repeatF(new StringF())
    printfn "%A" ((rs3 :> Format).Translate(Language.scala)) // TODO: why have to cast to a Format?!?

    // a larger example demonstrating the badness of not inferring on constructors.
    let monster = 
        new RepeatF<P2<StringF, S2<IntF, StringF>>>(
            new P2<StringF, S2<IntF, StringF>>(new StringF(), new S2<IntF, StringF>(IntF(), StringF())))
    printfn "%A" monster

    // and the better version
    let happier = repeatF(p2F(new StringF(), s2F(new IntF(), new StringF())))
    printfn "%A" happier

type BT = 
    | Leaf of string
    | Bin of (BT * BT)

//let btR: Reader<BT, Format> = 
//    let f: Reader<BT, Format> -> Reader<BT, Format> = 
//        (fun self -> 
//            let leafReader = stringR.Map(Leaf)
//            let binReader = p2R(self,self)(Bin)
//            union2R(leafReader, binReader) :> Reader<BT, Format>)
//    fixR(f) :> Reader<BT, Format>

checks()
simpleReadsAndWritesToByteArray()
simpleReadsAndWritesToFile()
formatTesting()
