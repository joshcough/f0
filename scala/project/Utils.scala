import java.io.{FileWriter, BufferedWriter, File}

// I'm sure stuff like this exists somewhere...
// But oh well for now.
object StringUtils {
  implicit def pimpString(s:String) = new {
    def replaceBetween(startDelim:String, endDelim:String, replacement:String): Option[String] = {
      val startIndex = s.indexOf(startDelim)
      val endIndex = s.indexOf(endDelim)
      if(startIndex > -1 && endIndex >= startIndex)
        Some(s.take(startIndex + startDelim.length) + replacement + s.takeRight(s.length - endIndex))
      else None
    }
    def replaceAfter(delim:String, replacement:String): Option[String] = {
      val startIndex = s.indexOf(delim)
      if(startIndex > -1) Some(s.take(startIndex + delim.length) + replacement) else None
    }
  }
}

object FileUtils {
  import StringUtils._
  implicit def pimpFile(file:File) = new {
    def read(): String = scala.io.Source.fromFile(file).getLines().mkString("\n")
    def write(s: String): File = {
      val w = new BufferedWriter(new FileWriter(file))
      w.write(s); w.close()
      file
    }
    def map(f: String => String): File = {
      val oldContents = read()
      val newContents = f(oldContents)
      if(newContents != oldContents) write(newContents) else file
    }
    def replaceBetween(startDelim:String, endDelim:String, replacement:String): File =
      map(s => s.replaceBetween(startDelim, endDelim, replacement).getOrElse(s))
    def replaceAfter(delim:String, replacement:String): File =
      map(s => s.replaceAfter(delim, replacement).getOrElse(s))
  }
}