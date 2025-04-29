package nlp4s.printer

import cats.data.Writer
import cats.syntax.all._

class StringPrinter {
  import StringPrinter._

  type F[T] = Writer[List[String], T]

  def isWord(s: String): Boolean = s.forall(_.isLetter)
  def isNumber(s: String): Boolean = s.forall(_.isDigit)
  def isPunctuation(s: String): Boolean = s.forall(punctuationChars.contains(_))
  def endSentence(s: String): Boolean = s.exists(sentenceEnder.contains(_))
  def prependSpace(s: String): Boolean = !isPunctuation(s)
  def concatString(s: String): Boolean = s.contains('_')

  def tell(s: String): F[Unit] = Writer.tell(List(s))

  def splitConcat(s: String): Iterable[String] =
    s.split('_')

  def writeTokens(isFirst: Boolean, capitalize: Boolean, it: Iterable[String]): F[Unit] = {
    it.headOption match {
      case None => Writer.value(())
      case Some(t) if concatString(t) => {
        writeTokens(isFirst, capitalize, splitConcat(t) ++ it.tail)
      }
      case Some(t) => {
        val prefix = if(isFirst) { "" } else { " " }
        val punct = it.tail.takeWhile(isPunctuation).mkString("")
        val token = (if(capitalize) { t.capitalize } else { t }) ++ punct
        val rest = it.tail.dropWhile(isPunctuation)

        tell(prefix ++ token) >> writeTokens(false, endSentence(punct), rest)
      }
    }
  }

  def write(strings: Iterable[String]): List[String] =
    writeTokens(true, true, strings.map(_.trim)).run._1

  def writeMkString(strings: Iterable[String]): String =
    write(strings).mkString("")
}

object StringPrinter {
  val punctuationChars: Set[Char] =
    Set('.', ',', '?', '!')

  val sentenceEnder: Set[Char] =
    Set('.', '?', '!')
}
