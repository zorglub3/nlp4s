package nlp4s.realiser

import nlp4s.base.NlpResult

abstract class Realiser[SentenceSpec] {
  def run(sentences: SentenceSpec): NlpResult[List[String]] = ???
}
