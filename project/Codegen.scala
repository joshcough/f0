import java.io.{BufferedWriter, FileWriter, File}
import FileUtils._

/**
 * Generates Scala Sum Readers, Writers and Formats for Sum2 - Sum22.
 */
object CodegenScala{
  // this is the main entry point from Build.scala
  def apply(baseDir:File) = new CodegenScala(new File(baseDir, "main/scala/f0")).codegen()
}

/**
 * Generates F# Sum Readers, Writers and Formats for Sum2 - Sum22.
 */
object CodegenFSharp{
  // this is the main entry point from Build.scala
  def apply(baseDir:File) = new CodegenFSharp(new File(baseDir, "../../fsharp/f0/f0")).codegen()
}

class CodegenScala(srcDir: File) {
  def codegen(): List[File] = List(
    writeCode("SumsReaders.scala", genSumReaders),
    writeCode("SumsWriters.scala", genSumWriters),
    writeCode("SumsFormats.scala", genSumFormats))

  def mapSums(f: SumGenNScala => String, sep:String="\n") = (2 to 22).map(n => f(new SumGenNScala(n))).mkString(sep)
  def writeCode(file:String, code:String) = new File(srcDir,file).write(code.stripMargin.trim)

  def genSumReaders = """
    |package f0
    |
    |import Formats._
    |
    |trait SumReaders {
    """ + mapSums(_.genReader) + "\n}"

  def genSumWriters = """
    |package f0
    |
    |import Formats._
    |
    |trait SumWriters {
    """ + mapSums(_.genWriter) + "\n}"

  def genSumFormats = """
    |package f0
    |
    |import Formats.Implicits.asFormat
    """ + mapSums(_.genFormat) + "\n" +
    "trait SumFormats {\n  " +
      mapSums(_.genFormatImplicit, sep="\n  ") +
    "\n}"
}

/**
 * Generator for the sum reader, writer, and format for a particular N.
 * For example: s5R, s5W, S5 (the format name)
 */
case class SumGenNScala(n:Int){
  val ns = (0 until n) toList
  def names(prefix:String) = ns map (prefix +)
  val types = names("A")
  val formats = names("F")
  val format = "S%s[%s]".format(n, formats.mkString(","))
  val varNames = types map (_.toLowerCase)
  val typeParams = types zip formats flatMap { case (a,f) => List(a,f) }

  def genReader: String = {
    val readerNames = names("r")
    val readers = readerNames zip types zip formats map { case ((name,a),f) => "%s: Reader[%s,%s]".format(name,a,f) }
    val fnames = names("f")
    val fargs = types.zipWithIndex map { case (t,i) => "f%s: %s => R".format(i, t) }
    val bindings = readerNames map (r => "val bind%s = %s.bind(s)".format(r,r))
    val cases = fnames.zip(readerNames).zipWithIndex map {
      case ((f,r),n) => "case %d => %s(bind%s.get)".format(n,f,r)
    }
    val s = """
    |  /** %d-way sum reader */
    |  def s%sR[%s,R](%s)(%s) = new Reader[R,%s] {
    |    def bind(s: Source): Get[R] = new Get[R] {
    |      %s
    |      def get = (s.readByte: @scala.annotation.switch) match {
    |        %s
    |        case x => sys.error("unrecognized tag byte: " + x)
    |      }
    |    }
    |  }""".format(
      n,
      n,
      typeParams.mkString(","),
      readers.mkString(", "),
      fargs.mkString(", "),
      format,
      bindings.mkString("\n      "),
      cases.mkString("\n        "))

    val u = """
    |  def union%sR[A,%s](%s): Reader[A,S%s[%s]] =
    |    s%sR(%s)(%s)""".format(
      n,
      formats.mkString(","),
      types.map(_.toLowerCase).zip(formats).map {
        case (t, f) => t + ": Reader[A," + f + "]"
      }.mkString(", "),
      n,
      formats.mkString(","),
      n,
      types.map(_.toLowerCase).mkString(", "),
      List.fill(n)("identity").mkString(", "))
    s + "\n" + u
  }

  def genWriter: String = {
    val writerNames = names("w")
    val writers = writerNames zip types zip formats map {
      case ((name,a),f) => "%s: Writer[%s,%s]".format(name,a,f)
    }
    val bindings = writerNames.zipWithIndex map { case (w,i) =>
      "val bind%s = { val b = %s.bind(o); ((a: A%d) => { o(%d:Byte); b(a).asInstanceOf[EffectW[%s]] }) }".format(w,w,i,i,format)
    }
    val conts = types map (a => "%s => EffectW[%s]".format(a,format)) mkString (", ")
    val s = """
    |  /** %d-way sum writer */
    |  def s%dW[%s,R](%s)
    |                (f: (%s) => R => EffectW[%s])
    |  = new Writer[R,%s] {
    |    def bind(o: Sink): R => EffectW[%s] = {
    |      %s
    |      f(%s)
    |    }}
    """.format(
      n,
      n,
      typeParams.mkString(","),
      writers.mkString(", "),
      conts,
      format,
      format,
      format,
      bindings.mkString("\n      "),
      writerNames map ("bind"+) mkString (", "))
    s
  }

  def genFormat = """
    |case class S%s[%s](%s) extends Format {
    |  override def translate(l: Language) = l.typeApplication("S%s", %s)
    |}""".format(
      n,
      types.map("+" +).mkString(","),
      varNames.zip(types).map{ case (l,u) => l + ": " + u }.mkString(", "),
      n,
      varNames.map(_ + " translate l").mkString(", "))

  def genFormatImplicit =
    "implicit def s%sF[%s](implicit %s) = S%s(%s)".format(
      n,
      types.mkString(","),
      varNames.zip(types).map{ case (l,u) => l + ": " + u }.mkString(", "),
      n,
      varNames.mkString(", "))
}

class CodegenFSharp(srcDir: File) {
  val startDelim = "(*** AUTO GENERATED CODE BELOW ***)"
  val endDelim = "(*** AUTO GENERATED CODE ABOVE ***)"

  def codegen(): List[File] = List(
    writeCode("SumFormatTypes.fs", genSumFormatTypes),
    writeCodeBetweenDelims("Writers.fs", genSumWriters),
    writeCodeBetweenDelims("Readers.fs", genSumReaders),
    writeCodeBetweenDelims("Formats.fs", genSumFormatFunctions)
  )

  def mapSums(f: SumGenNFSharp => String, sep:String="\n") = (2 to 22).map(n => f(new SumGenNFSharp(n))).mkString(sep)
  def writeCode(file:String, code:String) = new File(srcDir,file).write(code.stripMargin.trim)
  def writeCodeBetweenDelims(file:String, code:String) =
    new File(srcDir,file).replaceBetween(startDelim, endDelim, "\n" + code + "\n\n")

  def genSumFormatTypes = """
    |namespace f0
    |
    |open ForceFormat
    """ + mapSums(_.genFormatType)

  def genSumWriters = mapSums(_.genWriter)
  def genSumReaders = mapSums(_.genReader)
  def genSumFormatFunctions = mapSums(_.genFormatFunction)
}

/**
 * Generator for the sum reader, writer, and format for a particular N.
 * For example: s5R, s5W, S5 (the format name)
 */
case class SumGenNFSharp(n:Int){

  val ns = (0 until n) toList
  def names(prefix:String) = ns map (prefix +)
  val types = names("'a")
  val formats = names("'f")
  val format = "S%s<%s>".format(n, formats.mkString(","))
  val varNames = types map (_.drop(1).toLowerCase)
  val typeParams = types zip formats flatMap { case (a,f) => List(a,f) }

  def genFormatType = """
    |type S%s<%s>(%s) =
    |  interface Format with member f.Translate l =
    |    l.TypeApplication("S%s", [%s])""".format(
      n,
      types.mkString(","),
      varNames.zip(types).map{ case (l,u) => l + ": " + u }.mkString(", "),
      n,
      varNames.map("forceFormat(" + _ + ").Translate l").mkString("; "))

  def genFormatFunction = "    let s%dF(%s) = new S%d<%s>(%s)".format(
      n,
      varNames.zip(types).map{ case (l,u) => l + ": " + u }.mkString(", "),
      n,
      types.mkString(","),
      varNames.mkString(", "))
  
  def genWriter: String = {
    val writerNames = names("w")
    val writers = writerNames zip types zip formats map { case ((name,a),f) => "%s: Writer<%s,%s>".format(name,a,f) }
    val effect = "EffectW<%s>" format format
    val bindings = writerNames map { w => "let %sBound = %s.Bind(s)".format(w, w) }
    val funcs = writerNames.zipWithIndex map { case (w,i) =>
      "(fun a -> s.WriteByte(%duy); %sBound(a); ew)".format(i, w)
    }
    val conts = types map (a => "(%s -> %s)".format(a,effect)) mkString (" * ")

    val s = """
    |    (* %d-way sum writer *)
    |    let s%dW(%s)
    |            (f: (%s) -> 'r -> %s) = {
    |        new Writer<'r,%s>() with
    |            member w.Format = Formats.s%dF(%s)
    |            member w.Bind(s: Sink) =
    |                %s
    |                f(%s)
    |    }""".format(
      n,
      n,
      writers.mkString(", "),
      conts,
      effect,
      format,
      n,
      writerNames.map(_+".Format").mkString(", "),
      bindings.mkString("\n                "),
      funcs mkString (", "))
    s.stripMargin
  }

  def genReader: String = {
    val readerNames = names("r")
    val readers = readerNames zip types zip formats map { case ((name,a),f) => "%s: Reader<%s,%s>".format(name,a,f) }
    val fargs = types.zipWithIndex map { case (t,i) => "f%d: %s -> 'r".format(i,t) }
    val bindings = readerNames map { r => "let %sBound = %s.Bind(s)".format(r, r) }
    val matches = readerNames.zipWithIndex map { case (r,i) => "| %duy -> f%d(%sBound.Get)".format(i,i,r) }
    val reader = """
    |    (* %d-way sum reader *)
    |    let s%dR(%s)
    |            (%s) = {
    |        new Reader<'r,%s>() with member r.Bind(s: Source) =
    |            %s
    |            { new Get<'r> with member g.Get =
    |                match byteF(s) with
    """.stripMargin.format(
      n,
      n,
      readers.mkString(", "),
      fargs.mkString(", "),
      format,
      bindings.mkString("\n            ")) +
      matches.mkString ("            ", "\n                ", "") + ("""
    |                | x -> failwith("unrecognized tag byte: " + x.ToString())
    |            }
    |    }""".stripMargin)

    val union = "let union%dR(%s) = s%dR(%s)(%s)".format(
      n,
      readerNames.mkString(","),
      n,
      readerNames.mkString(","),
      List.fill(n)("id").mkString(",")
    )
    reader + "\n\n    " + union
  }
}