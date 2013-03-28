namespace f0

open System.Collections.Generic
open System.Linq;

module ReadersCS =

    let listR<'a, 'f>(r): Reader<List<'a>, RepeatF<'f>> = 
        Readers.listR(r).Map(Seq.ofList >> (fun xs -> xs.ToList()))

    let p2R (ra: Reader<'a,'f1>, rb: Reader<'b,'f2>, func : System.Func<'a,'b,'r>) = 
        Readers.p2R(ra, rb)(fun (a,b) -> func.Invoke(a,b))
