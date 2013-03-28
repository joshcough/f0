using System;
using System.Text;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using f0;

namespace F0CSharpTesting {
    [TestClass]
    public class F0ExamplesTests {

        Person j = new Person("Josh Cough", 10);

        [TestMethod]
        public void TestPersonEasy() {
            Person result = TestHelpers.roundTrip(j, F0Examples.personWriterT, F0Examples.personReader);
            Assert.AreEqual(j.Name, result.Name);
            Assert.AreEqual(j.Age, result.Age);
        }

        [TestMethod]
        public void TestPersonTypeInferenceNotWorking() {
            // here is the signature for p2R:
            //let p2R (ra: Reader<'a,'f1>, rb: Reader<'b,'f2>, func : System.Func<'a,'b,'r>)
            // here, i create a reader using p2R. it doesnt require type params.
            var r = ReadersCS.p2R(Readers.stringR, Readers.intR, (s, i) => new Person(s, i));

            // here is the definition of tuple2W:
            // let tuple2W(wa,wb) = p2W(wa,wb)(<|)
            // here, i create a writer using tuple2W. as you can see, it doesn't type params.
            var wt = Writers.tuple2W(Writers.stringW, Writers.intW).Cmapcs((Person p) => Tuple.Create(p.Name, p.Age));

            // here is the signature for p2W
            // let p2W(wa: Out<'a,'f1>, wb: Out<'b,'f2>, 
            // func: System.Func<System.Func<'a,'b, EfW<P2<'f1,'f2>>>, 'r, EfW<P2<'f1,'f2>>>): Writer<'r,P2<'f1,'f2>>
            // here, i create a writer using p2W. it DOES require type params. why?
            //var w = WritersCS.p2W<String, Formats.StringF, int, Formats.IntF, Person>(Writers.stringW, Writers.intW, (ab, p) =>
            //    ab(p.Name, p.Age));
            //var w = WritersCS.p2W(Writers.stringW, Writers.intW, (ab) => (Person p) => ab(p.Name, p.Age));
            var w = WritersCS.p2W(Writers.stringW, Writers.intW).Invoke<Person>((ab,p) => ab(p.Name, p.Age));
            // here is what it would look like without the types.
            // var w = WritersCS.p2W(Writers.stringW, Writers.intW, (ab, p) => ab(p.Name, p.Age));
            // which is definitely a win over the tuple form:
            // var wt = Writers.tuple2W(Writers.stringW, Writers.intW).cmapcs((Person p) => Tuple.Create(p.Name, p.Age));

            // this is really only a few characters smaller than using tuple2W and cmapcs.
            // so, if we never figure this problem out, i don't really think its that huge of an issue
            // one potential problem is that tuples only go up to T7 or T8 though. 
            
            Person result = TestHelpers.roundTrip(j, w, r);
            Assert.AreEqual(j.Name, result.Name);
            Assert.AreEqual(j.Age, result.Age);
        }
    }
}

// def tuple2W[A,F1,B,F2](wa: Writer[A,F1], wb: Writer[B,F2]) = p2W(wa,wb)(f => (p:(A,B)) => f(p._1,p._2))

