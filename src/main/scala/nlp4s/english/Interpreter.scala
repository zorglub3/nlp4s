package nlp4s.english

import nlp4s.base.NlpResult
import nlp4s.base.Tense
import nlp4s.base.Mode
import nlp4s.base.LinkTag
import nlp4s.english.lexicon.EnglishLinkTags
import nlp4s.english.lexicon.EnglishWordTags
import nlp4s.mrs.GraphInterpreter
import nlp4s.mrs.InterpreterError
import nlp4s.mrs.MRS
import nlp4s.mrs.Handle
import nlp4s.mrs.Relation
import nlp4s.mrs.Variable
import nlp4s.parser.Parser

class Interpreter extends GraphInterpreter {
  import cats.syntax.all._
  import EnglishLinkTags._
  import EnglishWordTags._

  def getVerbRelationName(root: String): Interpret[String] =
    pure(root) // TODO - get this from lexicon somehow

  def getQuantifierRelation(name: String): Interpret[String] =
    pure(name) // TODO - get this from lexicon

  def getNounRelation(name: String): Interpret[String] =
    pure(name) // TODO - get this from lexicon
 
  def verbTense(w: Int): Interpret[Tense] =
    collectFirstTag(w) { case EnglishWordTags.WordTense(tense) => tense }

  def verbRoot(w: Int): Interpret[String] =
    collectFirstTag(w) { case EnglishWordTags.VerbRoot(root) => root }

  def nounRoot(w: Int): Interpret[String] =
    collectFirstTag(w) { case EnglishWordTags.NounRoot(root) => root }

  def simpleTense(w: Int): Interpret[(Mode, Tense)] = {
    for {
      _ <- guardEmpty(graphEdgeFrom(EnglishLinkTags.T, w))
      _ <- guardEmpty(graphEdgeFrom(EnglishLinkTags.H, w))
      _ <- graphEdgeFrom(EnglishLinkTags.S, w) <+> graphEdgeFrom(EnglishLinkTags.Qs, w)
      tense <- verbTense(w)
    } yield (Mode.Declarative, tense)
  }
    
  def imperativeTense(w: Int): Interpret[(Mode, Tense)] = {
    for {
      _ <- guardEmpty(graphEdgeFrom(EnglishLinkTags.S, w))
      _ <- graphEdgeFrom(EnglishLinkTags.W, w)
      _ <- guardTokenHasTag(w, EnglishWordTags.RootForm)
    } yield (Mode.Imperative, Tense.Present)
  }

  def futureTense(w: Int): Interpret[(Mode, Tense)] = {
    // TODO check that subject/help-verb position
    // - is it interrogative or declarative

    for {
      h <- graphEdgeFrom(EnglishLinkTags.H, w)
      s <- graphEdgeFrom(EnglishLinkTags.S, w)
      r <- verbRoot(h)
      _ <- guard(r == "will")
      _ <- guardTokenHasTag(w, EnglishWordTags.RootForm)
    } yield (if(h < s) Mode.Interrogative else Mode.Declarative, Tense. Present)
  }

  def verbPhraseTense(w: Int): Interpret[(Mode, Tense)] =
    simpleTense(w) <+> imperativeTense(w) <+> futureTense(w)

  def negation(w: Int): Interpret[Boolean] = 
    graphEdgeFrom(EnglishLinkTags.N, w).flatMap(_ => pure(true)) <+> pure(false)

  type BuildVerbRelation = 
    (Option[Variable], Option[Variable], Option[Variable]) => Interpret[Handle]
    //(Option[Variable], Option[Variable], Option[Variable]) => Option[(Mode, Tense, Boolean, Relation[Handle])]

  def verb(w: Int): Interpret[BuildVerbRelation] = {
    for {
      r <- verbRoot(w)
      f <- getVerbRelationName(r)
      t <- verbPhraseTense(w)
      mode = t._1
      tense = t._2
      n <- negation(w)
    } yield {
      case (None, Some(obj), Some(biObj)) if mode == Mode.Imperative => {
        for {
          v <- makeVariable()
          h <- makeHandle()
          _ <- addRelation(h, Relation.BitransitiveVerb(f, v, obj, biObj))
        } yield h
      }
      case (None, Some(obj), None) if mode == Mode.Imperative => {
        for {
          v <- makeVariable()
          h <- makeHandle()
          _ <- addRelation(h, Relation.TransitiveVerb(f, v, obj))
        } yield h 
      }
      case (None, None, _) if mode == Mode.Imperative => {
        for {
          v <- makeVariable()
          h <- makeHandle()
          _ <- addRelation(h, Relation.IntransitiveVerb(f, v))
        } yield h
      }
      case (Some(subj), Some(obj), Some(biObj)) => {
        for {
          h <- makeHandle()
          _ <- addRelation(h, Relation.BitransitiveVerb(f, subj, obj, biObj))
        } yield h
      }
      case (Some(subj), Some(obj), None) => {
        for {
          h <- makeHandle()
          _ <- addRelation(h, Relation.TransitiveVerb(f, subj, obj))
        } yield h
      }
      case (Some(subj), None, _) => {
        for {
          h <- makeHandle()
          _ <- addRelation(h, Relation.IntransitiveVerb(f, subj))
        } yield h
      }
      case _ => fail() 
    }
  }

  // TODO - support for bi-transitive verbs (parse second object)
  // TODO - support for adverbs and prepositions
  // TODO - variables for verb-relations and add tense- and mode-predicates to those
  //        (see MRS intro doc section 6.1.3)
  def verbPhrase(mainVerb: Int): Interpret[Unit] = {
    for {
      _ <- guardTokenHasTag(mainVerb, Verb)
      vf <- verb(mainVerb)
      subj <- toOption(nounPhraseFrom(EnglishLinkTags.S, mainVerb))
      obj <- toOption(nounPhraseFrom(EnglishLinkTags.O, mainVerb))
      handle <- vf(subj.map(_._3), obj.map(_._3), None)
      _ <- optional(subj.map(s => addConstraint(s._1, handle)))
      _ <- optional(obj.map(o => addConstraint(o._1, handle)))
    } yield ()
  }

  type MakeQuantifier = (Handle, Handle) => Interpret[(Handle, Variable)]

  def determinerFrom(w: Int): Interpret[MakeQuantifier] = {
    for {
      d <- graphEdgeFrom(EnglishLinkTags.D, w)
      word <- word(d).map(_.toLowerCase)
      rel <- getQuantifierRelation(word)
      v <- makeVariable()
    } yield { (h1, h2) =>
      for {
        h0 <- makeHandle()
        _ <- addRelation(h0, Relation.Quantifier(rel, v, h1, h2))
      } yield (h0, v)
    }
  }

  type MakeRelations = Variable => List[Relation[Handle]]

  def adjectivesFrom(w: Int): Interpret[MakeRelations] = {
    pure(_ => List.empty)
  }

  def prepositionsFrom(w: Int): Interpret[MakeRelations] = {
    pure(_ => List.empty)
  }

  def countNounPhrase(w: Int): Interpret[(Handle, Handle, Variable)] = {
    for {
      _ <- guardTokenHasTag(w, EnglishWordTags.CountNoun)
      root <- nounRoot(w)
      npRel <- getNounRelation(root)
      makeQuantifier <- determinerFrom(w)
      makeAdjectives <- adjectivesFrom(w)
      makePrepositions <- prepositionsFrom(w)
      qh1 <- makeHandle()
      qh2 <- makeHandle()
      p <- makeQuantifier(qh1, qh2)
      (quantifierHandle, quantifierVariable) = p
      r1 = Relation.CountNoun[Handle](npRel, quantifierVariable) :: (makeAdjectives(quantifierVariable) ++ makePrepositions(quantifierVariable))
      rh1 <- makeHandle()
      _ <- addRelationBag(rh1, r1)
      _ <- addConstraint(qh1, rh1)
    } yield (qh2, quantifierHandle, quantifierVariable)
  }

  def pronounPhrase(w: Int): Interpret[(Handle, Handle, Variable)] =
    fail() // TODO

  def massNounPhrase(w: Int): Interpret[(Handle, Handle, Variable)] =
    fail() // TODO

  def nounPhraseFrom(tag: LinkTag, verb: Int): Interpret[(Handle, Handle, Variable)] = {
    for {
      n <- graphEdgeFrom(tag, verb)
      np <- countNounPhrase(n) <+> pronounPhrase(n) <+> massNounPhrase(n)
    } yield np
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

  val interpreter = statement <+> question <+> participle <+> imperative

  def run(words: Vector[String], parserOutput: Parser.Output): NlpResult[MRS] = {
    interpreter.runS(init(words, parserOutput._1, parserOutput._2)) match {
      case Some(state) => Right(state.mrsBuilder.result())
      case None => Left(InterpreterError("Could not interpret sentence"))
    }
  }

  def runMany(words: Vector[String], parseOutputs: List[Parser.Output]): NlpResult[List[MRS]] = {
    parseOutputs.flatMap { po => interpreter.runS(init(words, po._1, po._2)) } match {
      case l@(_::_) => Right(l.map(_.mrsBuilder.result()))
      case Nil => Left(InterpreterError("Could not interpret any sentence parse"))
    }
  }
}

