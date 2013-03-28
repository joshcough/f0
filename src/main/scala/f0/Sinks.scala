package f0

import java.io.FileOutputStream

trait Sink {
  def writeBit(b: Boolean): Unit
  def apply(b: Byte): Unit
  def flush: Unit
  def close: Unit
  def state: Reg
  def using[A](handler: Sink => A): A = try handler(this) finally this.close
  def finishWith[A,F](t: (Writer[A,F], A)) = this.using(s => t._1.bind(s)(t._2))
}

object Sinks {

  def using[A](s:Sink)(handler: Sink => A) = s.using(handler)

  def toFile(filename:String): Sink = toOutputStream(new FileOutputStream(filename))

  def toProcess(p:Process, bufSize: Int = 4096): Sink =
    toOutputStream(new java.io.BufferedOutputStream(p.getOutputStream, bufSize))

  def toOutputStream(os: java.io.OutputStream): Sink = new Sink {
    var i = 0
    var buf = new Array[Byte](256)
    val reg = Reg(Map())
    var bitOffset = 1
    var byteBuf = 0
    def writeBit(b: Boolean): Unit = {
      //println("writing " + b + ", offset: " + bitOffset)
      if (bitOffset == 256) {
        //println("flushing byte buf: " + byteBuf)
        bitOffset = 1
        apply(byteBuf.asInstanceOf[Byte])
        bitOffset = 2
        byteBuf = if (b) 1 else 0
      }
      else {
        if (b) byteBuf |= bitOffset
        bitOffset *= 2
        //println ("buf: " + byteBuf)
      }
    }
    def apply(b: Byte): Unit = {
      if (bitOffset != 1) {
        bitOffset = 1
        apply(byteBuf.asInstanceOf[Byte])
        byteBuf = 0
      }
      if (i == 256) { os.write(buf); i = 0 }
      buf(i) = b; i += 1
    }
    
    def flush: Unit = {
      if (bitOffset != 1) {
        bitOffset = 1
        //println("flushing byte buf on done: " + byteBuf)
        apply(byteBuf.asInstanceOf[Byte])
        byteBuf = 0
      }
      if (i != 0) {
        os.write(buf, 0, i)
        i = 0
      }
    }
    
    def close: Unit = {
      flush
      os.close()
    }
    def state: Reg = reg
  }
}
