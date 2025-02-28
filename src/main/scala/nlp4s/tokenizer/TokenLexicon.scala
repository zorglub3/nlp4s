package nlp4s.tokenizer

import collection.mutable.{HashSet, MultiDict}

class TokenLexicon(
  ignoreCase: Boolean,
  tokens: Set[String], 
  concatTokens: MultiDict[String, List[String]]
) {
  def contains(s: String): Boolean = {
    val tt = if(ignoreCase) { s.toLowerCase() } else { s }
    tokens.contains(tt)
  }

  def lookup(s: String): Option[String] = {
    val tt = if(ignoreCase) { s.toLowerCase() } else { s }
    if(tokens.contains(tt)) { Some(s) } else { None }
  }

  def concat(s: String): List[List[String]] = {
    val tt = if(ignoreCase) { s.toLowerCase() } else { s }
    concatTokens.get(tt).toList
  }
}

object TokenLexicon {
  def newBuilder(ignoreCase: Boolean) = new Builder(ignoreCase)

  class Builder(ignoreCase: Boolean) {
    val tokens = HashSet.empty[String]
    val concatTokens = MultiDict.empty[String, List[String]]

    def add(s: String): Unit = {
      val tt = if(ignoreCase) { s.toLowerCase() } else { s }

      if(tt.contains('_')) {
        val parts = tt.split('_').toList
        concatTokens.addOne(parts.head -> parts.tail)
      }

      tokens.add(tt)
    }

    def result(): TokenLexicon = {
      // TODO: make sure concatTokens are ordered correctly - longest first
      new TokenLexicon(ignoreCase, tokens.toSet, concatTokens)
    }
  }
}
