namespace f0

open Helpers

// Syntax for type constructor application in this language. 
// Example: C/Java/C++/C#, `typeApplication("Foo","A","B")` is `Foo<A,B>`.
type TypeApplication = (string * List<string>) -> string
type Qualify = (string * string) -> string

type Language = 
    | Language of TypeApplication * Qualify
    member self.TypeApplication = match self with | Language(ta, _) -> ta
    member self.Qualified namespac = match self with
    | Language(ta,q) -> Language((fun (f, args) -> ta(q(f, namespac), (args |> List.map (fun n -> q(n, namespac))))), q)
    static member DotQualifiedLang ta = 
        Language(ta, (fun (name, namespac) -> namespac + "." + name))
    // Languages that use pointy brackets for type constructor application (`Foo<A,B,C>`), e.g. Java, C#, C++.
    static member pointyBrackets() = 
        Language.DotQualifiedLang (fun (f, args) -> sprintf "%s<%s>" f (mkString args ", "))
    // Languages that use square brackets for type constructor application (`Foo[A,B,C]`), e.g. Scala.
    static member squareBrackets() = 
        Language.DotQualifiedLang (fun (f, args) -> sprintf "%s[%s]" f (mkString args ", "))
    //  Languages that use apposition for type constructor application (`(Foo A B)`), e.g. Haskell.
    static member apposition() = 
        Language.DotQualifiedLang (fun (f, args) -> sprintf "(%s %s)" f (mkString args ", "))
    static member csharp = Language.pointyBrackets()
    static member scala = Language.squareBrackets()
    static member java = Language.pointyBrackets()
    static member fsharp = Language.apposition()
