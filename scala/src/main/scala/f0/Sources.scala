package f0

trait ByteReader {
  def readByte: Byte
  def readBytes(n: Int): Array[Byte]
}

trait BitReader {
  def readBit: Boolean
}

trait Source extends ByteReader with BitReader {
  def apply[A,F](r:Reader[A,F]) = r.bind(this).get
  // TODO: these arent used, so im commenting them out - JC 11/29/11
  //def foldBytes[A](n: Int, z: A)(f: (A,Byte) => A): A
  //def state: Reg
}

object Sources {
  
  def getBit(b: Int, i: Int): Boolean = (b & i) != 0

  def fromFile(filename:String): Source = fromInputStream(new java.io.FileInputStream(filename))
  def fromProcess(p:Process): Source = fromInputStream(p.getInputStream)
  def fromArray(bytes: Array[Byte], start: Int = 0): Source = new SourceInternal {
    var byteIndex = start
    def readByte = { val b = bytes(byteIndex); byteIndex += 1; b }
    def readBytes(n: Int) = { 
      val a = new Array[Byte](n)
      Array.copy(bytes, byteIndex, a, 0, n)
      byteIndex += n
      a 
    }
// TODO: these arent used, so im commenting them out - JC 11/29/11
//    def foldBytes[A](n: Int, z: A)(f: (A,Byte) => A): A = {
//      var c = n
//      var acc = z
//      while (c > 0) { acc = f(acc, bytes(i)); i += 1; c -= 1 }
//      acc
//    }
//    val state = Reg(Map())
  }

  /**
   * reads from the stream, blocking until data is available
   * or throwing an EOFException if there is an attempt to read a byte
   * when the stream has reached EOF.
   **/
  def fromInputStream(is: java.io.InputStream): Source = new SourceInternal {
    var byteIndex = 0
    // TODO: this can probably be made more efficient,
    // because im just reading one byte at a time.
    // i could read into an array instead.
    val bytesStream = Iterator.continually({
      val i = is.read
      if(i == -1) throw new java.io.EOFException()
      i.toByte
    })
    def readByte = { val b = bytesStream.take(1).next(); byteIndex += 1; b }
    def readBytes(n: Int) = { byteIndex += n; bytesStream.take(n).toArray }
  }

  private trait SourceInternal extends Source with ByteReaderInternal with BitReaderInternal

  private trait ByteReaderInternal extends ByteReader {
    // byteIndex used to communicate with the BitReader
    // so it knows if it needs to read the next byte or not
    // TODO: i imagine theres a better way to do this.
    var byteIndex: Int
  }

  private trait BitReaderInternal extends ByteReaderInternal with BitReader {
    var bitOffset = 1
    var byteBuf = 0
    var byteBufMark = -1
    def readBit =
      if (byteBufMark != byteIndex || bitOffset == 256) {
        bitOffset = 2
        byteBuf = readByte
        byteBufMark = byteIndex
        getBit(byteBuf, 1)
      }
      else {
        val r = getBit(byteBuf,bitOffset)
        bitOffset *= 2
        r
      }
  }
}
