package nlp4s.english

import nlp4s.base.NlpResult
import nlp4s.english.lexicon.EnglishLinkTags
import nlp4s.english.lexicon.EnglishWordTags
import nlp4s.mrs.GraphInterpreter
import nlp4s.mrs.InterpreterError
import nlp4s.mrs.MRS
import nlp4s.parser.Parser

class Interpreter extends GraphInterpreter {
  import cats.syntax.all._
  import EnglishLinkTags._
  import EnglishWordTags._

  def verbPhrase(mainVerb: Int): Interpret[Unit] = {
    for {
      _ <- guardTokenHasTag(mainVerb, Verb)
    } yield ???
  }

  def statement: Interpret[Unit] = {
    for {
      p <- graphEdge(S)
      _ <- verbPhrase(p._1) <+> verbPhrase(p._2)
    } yield ()
  }

  def question: Interpret[Unit] = {
    for {
      p <- graphEdge(Qs)
      _ <- verbPhrase(p._1) <+> verbPhrase(p._2)
    } yield ()
  }

  def participle: Interpret[Unit] = {
    for {
      p <- graphEdge(T)
      _ <- verbPhrase(p._1) <+> verbPhrase(p._2)
    } yield ()
  }

  def imperative: Interpret[Unit] = {
    for {
      p <- graphEdge(W)
      _ <- verbPhrase(p._2)
    } yield ()
  }

  def run(parserOutput: Parser.Output): NlpResult[MRS] = {
    val interpreter = statement <+> question <+> participle <+> imperative

    interpreter.runS(init(parserOutput._1, parserOutput._2)) match {
      case Some(state) => Right(state.mrsBuilder.result())
      case None => Left(InterpreterError("Could not interpret sentence"))
    }
  }
}

