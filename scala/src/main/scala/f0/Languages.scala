package f0

trait Language { self =>
  /** Syntax for type constructor application in this language.
    * Example: C/Java/C++/C#, `typeApplication("Foo","A","B")` is `Foo<A,B>`. */
  def typeApplication(f: String, args: String*): String

  def qualify(name: String, namespace: String) = namespace + "." + name

  /** Change this language to use qualified names for identifiers. */
  def qualified(namespace: String): Language = new Language {
    def typeApplication(f: String, args: String*) =
      self.typeApplication(qualify(f,namespace), args.map(qualify(_,namespace)): _*)
    override def qualify(name: String, namespace: String) = self.qualify(name, namespace)
  }
}

object Languages {
  def canonical: Language = new Language {
    def flattenInProduct(s: String) =
      if (s.startsWith("product")) fmt(s.substring(2,s.length-1))
      else s
    def fmt(s: String) =
      if (s.last == 'F') s.init.toLowerCase
      else s
    def typeApplication(f: String, args: String*) =
      if (f == "P2") "product[" + args.map(flattenInProduct).mkString(", ") + "]"
      else if (f.startsWith("S") && f(1).isDigit) "sum[" + args.map(fmt).mkString(", ") + "]"
      else if (args.length == 0) fmt(f)
      else fmt(f) + "["+args.map(fmt).mkString(", ") + "]"
  }
  /** Languages that use pointy brackets for type constructor application (`Foo<A,B,C>`), e.g. Java, C#, C++. */
  def pointyBrackets: Language = new Language {
    def typeApplication(f: String, args: String*) = "%s<%s>".format(f,args.mkString(", ")) }

  /** Languages that use square brackets for type constructor application (`Foo[A,B,C]`), e.g. Scala */
  def squareBrackets: Language = new Language {
    def typeApplication(f: String, args: String*) = "%s[%s]".format(f,args.mkString(", ")) }

  /** Languages that use apposition for type constructor application (`(Foo A B)`), e.g. Haskell */
  def apposition: Language = new Language {
    def typeApplication(f: String, args: String*) = "(%s %s)".format(f,args.mkString(" ")) }

  val csharp = pointyBrackets
  val scala = squareBrackets
  val java = pointyBrackets
  val fsharp = pointyBrackets
}
