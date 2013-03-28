using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using f0;

// TODO: figure out if there is a way to use a namespace or something
// so that we don't have to qualify everything. (eg Writers.Writer. ick)
namespace F0CSharpTesting {
    [TestClass]
    public class BasicTests {
        [TestMethod]
        public void TestInt() {
            Assert.AreEqual(6, (TestHelpers.roundTrip(6, Writers.intW, Readers.intR)));
        }
        [TestMethod]
        public void TestTuple() {
            var result = TestHelpers.roundTrip(
                Tuple.Create(5, 6),
                Writers.tuple2W(Writers.intW, Writers.intW),
                Readers.tuple2R(Readers.intR, Readers.intR));
            Assert.AreEqual(Tuple.Create(5, 6),result);
        }
        [TestMethod]
        public void TestList() {
            List<int> ints = new List<int> { 1, 2, 3, 4, 5 };
            var result = TestHelpers.roundTrip(ints,
                WritersCS.repeatW(Writers.intW),
                ReadersCS.listR(Readers.intR));
            CollectionAssert.AreEquivalent(ints, result);
        }
    }
}