namespace f0

open System.IO

type Source =
  abstract member ReadBit:  bool
  abstract member ReadByte: byte
  abstract member ReadBytes: int -> array<byte>
  //def foldBytes[A](n: Int, z: A)(f: (A,Byte) => A): A
  abstract member State: Reg

type Binary<'f> = 
    abstract member source: Source
    // can map over Binary[F] to change its format!

module GetBit =
    let getBit (b:int) (i:int) : bool = not ((b &&& i) = 0)

open GetBit

// TODO both ByteArraySource and InputStream source share (copy/paste)
// the same bit reading code. figure out a way to share it.
type ByteArraySource(bytes:array<byte>, start:int) as self = 
    let mutable i = start
    let mutable bitOffset = 1
    let mutable byteBuf = 0
    let mutable byteBufMark = -1
    member s.ReadByte =
        let b = bytes.[i]
        i <- i + 1
        b
    interface Source with
        member s.ReadBit: bool =
            if byteBufMark <> i || bitOffset = 256
            then
                bitOffset <- 2
                byteBuf <- int self.ReadByte
                byteBufMark <- i
                getBit byteBuf 1
            else
                let r = getBit byteBuf bitOffset
                bitOffset <- bitOffset * 2
                r
        member s.ReadByte = self.ReadByte
        member s.ReadBytes n =
            let a = Array.create n 0uy
            System.Array.Copy(bytes, i, a, 0, n)
            i <- i + n
            a 
        member s.State = Reg Map.empty

type InputStreamSource(stream:Stream) as self = 
    let mutable byteIndex = 0
    let mutable bitOffset = 1
    let mutable byteBuf = 0
    let mutable byteBufMark = -1
    member s.ReadByte =
        let i: int = stream.ReadByte()
        if i = -1 then failwith("end of stream")
        let b: byte = (byte i)
        byteIndex <- byteIndex + 1
        b
    interface Source with
        member s.ReadBit: bool =
            if byteBufMark <> byteIndex || bitOffset = 256
            then
                bitOffset <- 2
                byteBuf <- int self.ReadByte
                byteBufMark <- byteIndex
                getBit byteBuf 1
            else
                let r = getBit byteBuf bitOffset
                bitOffset <- bitOffset * 2
                r
        member s.ReadByte = self.ReadByte
        member s.ReadBytes n =
            let a = Array.create n 0uy
            for i in 0 .. (n-1) do a.[i] <- s.ReadByte
            a 
        member s.State = Reg Map.empty

module Sources =

    let FromByteArray(bytes:array<byte>): Source = new ByteArraySource(bytes, 0) :> Source
    let FromInputStream(stream:Stream): Source = new InputStreamSource(new BufferedStream(stream)) :> Source
    let FromFile(filename:string): Source = FromInputStream(File.OpenRead(filename))
    let FromStdIn() = FromInputStream(System.Console.OpenStandardInput())
