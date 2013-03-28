namespace f0

[<AbstractClass>]
type Sink() =
  abstract member WriteBit: bool -> unit
  abstract member WriteByte:    byte -> unit
  abstract member Finished: unit -> unit
  abstract member State: Reg
  member this.Using (f: Sink -> 'a): 'a =
    let a = f(this)
    this.Finished()
    a

type OutputStreamSink(os: System.IO.Stream) as self = 
    inherit Sink()
    let mutable i = 0
    let mutable buf = Array.create 256 0uy
    let reg = Reg Map.empty
    let mutable bitOffset = 1
    let mutable byteBuf = 0
    override o.WriteByte(b:byte) =
        if bitOffset <> 1
        then
            bitOffset <- 1
            o.WriteByte(byte byteBuf)
            byteBuf <- 0
        if(i=256) then
            os.Write(buf, 0, buf.Length)
            i <- 0
        buf.[i] <- b
        i <- i + 1
    override s.WriteBit (b:bool): unit =
        if(bitOffset = 256)
        then
            bitOffset <- 1
            self.WriteByte(byte byteBuf)
            bitOffset <- 2
            byteBuf <- if b then 1 else 0
        else
            if(b) then byteBuf <- byteBuf ||| bitOffset
            bitOffset <- bitOffset * 2
        ()
    override s.Finished(): unit =
        if bitOffset <> 1 then
            bitOffset <- 1
            self.WriteByte(byte byteBuf)
            byteBuf <- 0
        if i <> 0 then os.Write(buf, 0, i)
        os.Close()
    override s.State: Reg = reg

module Sinks =

    let ToOutputStream(os: System.IO.Stream): Sink = new OutputStreamSink(os) :> Sink
    let ToFile(filename:string): Sink = ToOutputStream(System.IO.File.OpenWrite(filename))
    let ToStdOut() = ToOutputStream(System.Console.OpenStandardOutput())
    let ToStdErr() = ToOutputStream(System.Console.OpenStandardError())
