package nlp4s.tokenizer

import nlp4s.base.NlpResult
import nlp4s.base.Constants.LeftWall
import java.util.StringTokenizer
import cats.data.StateT
import cats.Eval

class Tokenizer(lexicon: TokenLexicon, delimiters: String) {
  private type Tokenize[A] = StateT[Eval, TokenizerState, A]

  private case class TokenizerState(
    tokens: List[String],
    unrecognizedTokens: List[String]
  ) {
    def addToken(t: String): TokenizerState = 
      copy(tokens = t :: tokens)
    def addUnrecognizedToken(t: String): TokenizerState = 
      copy(unrecognizedTokens = t :: unrecognizedTokens)
  }

  private def init(): TokenizerState = TokenizerState(List.empty, List.empty)

  private def unrecognizedToken(token: String): Tokenize[Unit] =
    StateT.modify(_.addUnrecognizedToken(token))

  private def addToken(token: String): Tokenize[Unit] =
    StateT.modify(_.addToken(token))

  private def scanTokens(tokens: List[String]): Tokenize[Unit] = {
    tokens match {
      case Nil => StateT.pure(())
      case h::t => {
        val concatTokens = lexicon.concat(h)
        concatTokens.find(t.startsWith(_)) match {
          case Some(tail) => continueScan((h :: tail).mkString("_"), t.drop(tail.length))
          case None => continueScan(h, t)
        }
      }
    }
  }

  private def continueScan(t: String, rest: List[String]): Tokenize[Unit] = {
    for {
      _ <- lexicon.lookup(t).fold(unrecognizedToken(t))(addToken)
      _ <- scanTokens(rest)
    } yield ()
  }

  def run(s: String): NlpResult[List[String]] = {
    val stringTokens = new StringTokenizer(s, delimiters)
    val builder = List.newBuilder[String]

    while(stringTokens.hasMoreTokens) {
      builder += stringTokens.nextToken
    }

    scanTokens(builder.result()).runS(init()).value match {
      case TokenizerState(tokens, Nil) => Right(LeftWall :: tokens.reverse)
      case TokenizerState(_, l @ h::t) => Left(UnrecognizedTokens(l.distinct))
    }
  }
}
