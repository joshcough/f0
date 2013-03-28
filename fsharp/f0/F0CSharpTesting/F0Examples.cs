using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using f0;

namespace F0CSharpTesting {

    class Person {
        private string myName = "N/A"; private int myAge = 0;
        public Person(string name, int age) { this.myName = name; this.myAge = age; }
        public string Name { get { return myName; } }
        public int Age { get { return myAge; } }
        public override string ToString() { return "Name = " + Name + ", Age = " + Age; }
    }

    class F0Examples {
        static public Reader<Person, P2<StringF, IntF>> personReader =
            ReadersCS.p2R(Readers.stringR, Readers.intR, (s, i) => new Person(s, i));

        static public Reader<Person, P2<StringF, IntF>> personReaderT = 
            Readers.tuple2R(Readers.stringR, Readers.intR).Mapcs<Person>((tup) => new Person(tup.Item1, tup.Item2));
        static public Writer<Person, P2<StringF, IntF>> personWriterT =
            Writers.tuple2W(Writers.stringW, Writers.intW).Cmapcs((Person p) => Tuple.Create(p.Name, p.Age));
    }
}
