package nlp4s.demo

import nlp4s.base.NlpResult
import nlp4s.english._
import nlp4s.english.std._
import nlp4s.mrs.MRS
import nlp4s.mrs.QuantifierScope
import nlp4s.parser.Parser
import nlp4s.tokenizer.Tokenizer
import nlp4s.realiser.Clause

class EnglishDemo {
  val lexiconBuilder = {
    new Lexicon.Builder(true) 
      with StandardAdjectives
      with StandardAdverbs
      with StandardNouns
      with StandardPrepositions
      with StandardVerbs
      with StandardWords
  }
  
  val lexicon = lexiconBuilder.result()
  val delimiters = " \n\t;.,!?"

  val tokenizer = new Tokenizer(lexicon.tokenLexicon, delimiters)
  val parser = new Parser(lexicon.ruleMap)
  val interpreter = new EnglishGraphInterpreter
  val quantifierScope = new QuantifierScope
  val realiser = new EnglishRealiser(lexicon.wordBook)

  def parseString(s: String): NlpResult[List[Parser.Output]] = {
    for {
      tokens <- tokenizer.run(s)
      parse <- parser.run(tokens)
    } yield parse
  }

  def interpretString(s: String): NlpResult[List[MRS]] = {
    for {
      tokens <- tokenizer.run(s)
      parse <- parser.run(tokens)
      mrs <- interpreter.runMany(tokens, parse)
    } yield mrs
  }

  def roundTrip(s: String): NlpResult[List[String]] = {
    for {
      tokens <- tokenizer.run(s)
      parse <- parser.run(tokens)
      mrs <- interpreter.runMany(tokens, parse)
      ast = quantifierScope.resolve(mrs.head)
      words <- realiser.run(List(Clause.MRSClause(ast.head)))
    } yield words
  }
}


