package nlp4s.tokenizer

import nlp4s.base.NlpError

case class UnrecognizedTokens(tokens: List[String]) extends NlpError(s"Unrecognized words: ${tokens.mkString(", ")}")
