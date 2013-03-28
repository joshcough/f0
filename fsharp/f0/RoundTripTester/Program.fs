open f0
open f0.Readers
open f0.Writers
open f0.Formats
open f0.Sinks
open f0.Sources

// This file is called from the scala test suite, in FSharpIntegrationTest
// That pipes stuff into the stdin of this process. We read it here, 
// and then write it all back out. The scala side reads it back in, 
// and makes sure that we get the same data back.

// (given that info, don't try to just run this process by itself, 
// it'll just sit there, waiting for data)

let r = listR(tuple3R(
    doubleR,
    tuple6R(intR, boolR, boolR, intR, tuple3R(longR, longR, longR), stringR), 
    tuple6R(doubleR, singleR, listR(intR), tuple2R(intR, intR), tuple3R(intR, intR, intR), streamR(intR).Map(Seq.ofList))))

let w = repeatW(tuple3W(
    doubleW,
    tuple6W(intW, boolW, boolW, intW, tuple3W(longW, longW, longW), stringW), 
    tuple6W(doubleW, singleW, repeatW(intW), tuple2W(intW, intW), tuple3W(intW, intW, intW), streamW(intW))))

Sinks.ToStdOut().Using(fun s -> w.Bind s (r.Bind(Sources.FromStdIn()).Get))
