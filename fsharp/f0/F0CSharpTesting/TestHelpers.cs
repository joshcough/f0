using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using f0;

namespace F0CSharpTesting {
    class TestHelpers {
        static public A roundTrip<A, F>(A a, Writer<A, F> w, Reader<A, F> r) {
            var ms = new System.IO.MemoryStream();
            var sink = Sinks.ToOutputStream(ms);
            w.Bind(sink).Invoke(a);
            sink.Finished();
            // NOTE: don't remove this. its super useful.
            //Console.Write("bytes: " + ms.ToArray().Select((x) => x.ToString()).Aggregate((x,y) => x + ", " + y) + "\n");
            var o = r.Bind(Sources.FromByteArray(ms.ToArray())).Get;
            return o;
        }
    }
}
