namespace f0

type Format =
    abstract member Translate: Language -> string

type UnitF()    = interface Format with member this.Translate l = "UnitF"
type ShortF()   = interface Format with member this.Translate l = "ShortF"
type IntF()     = interface Format with member this.Translate l = "IntF"
type DoubleF()  = interface Format with member this.Translate l = "DoubleF"
type FloatF()   = interface Format with member this.Translate l = "FloatF"
type StringF()  = interface Format with member this.Translate l = "StringF"
type BooleanF() = interface Format with member this.Translate l = "BooleanF"
type LongF()    = interface Format with member this.Translate l = "LongF"
type ByteF()    = interface Format with member this.Translate l = "ByteF"

module ForceFormat = 
  let forceFormat<'a>(a:'a) = match box a with 
    | :? Format as f -> f 
    | bad -> failwith ("not a format: " + bad.ToString())

