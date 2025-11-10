package nlp4s.demo

import nlp4s.base.NlpResult
import nlp4s.english._
import nlp4s.english.std._
import nlp4s.mrs.MRS
import nlp4s.mrs.AST
import nlp4s.mrs.QuantifierScope
import nlp4s.parser.Parser
import nlp4s.tokenizer.Tokenizer
import nlp4s.realiser.Clause
import nlp4s.printer.StringPrinter

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
  val printer = new StringPrinter

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

  def interpretStringAST(s: String): NlpResult[List[AST]] = {
    for {
      tokens <- tokenizer.run(s)
      parse <- parser.run(tokens)
      mrs <- interpreter.runMany(tokens, parse)
      ast <- quantifierScope.resolve(mrs.head)
    } yield ast.toList 
  }

  def roundTrip(s: String): NlpResult[List[String]] = {
    for {
      tokens <- tokenizer.run(s)
      parse <- parser.run(tokens)
      mrs <- interpreter.runMany(tokens, parse)
      ast <- quantifierScope.resolve(mrs.head)
      words <- realiser.run(List(Clause.MRSClause(ast.head)))
    } yield words
  }

  def roundTripString(s: String): NlpResult[String] = {
    for {
      tokens <- tokenizer.run(s)
      parse <- parser.run(tokens)
      mrs <- interpreter.runMany(tokens, parse)
      ast <- quantifierScope.resolve(mrs.head)
      words <- realiser.run(List(Clause.MRSClause(ast.head)))
    } yield printer.writeMkString(words)
  }

  def ppMRS(result: NlpResult[List[MRS]]): Unit = {
    result match {
      case Left(error) => println(s"Error: $error")
      case Right(mrsList) => mrsList.foreach(nlp4s.mrs.MRS.pp)
    }
  } 

  def ppAST(result: NlpResult[List[AST]]): Unit = {
    result match {
      case Left(error) => println(s"Error: $error")
      case Right(astList) => astList.foreach(nlp4s.mrs.AST.pp)
    }
  } 

  def ppString(result: NlpResult[String]) = {
    result match {
      case Left(error) => println(s"Error: $error")
      case Right(str) => println(s"Result: $str")
    }
  } 
}


