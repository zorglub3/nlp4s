package nlp4s.english

import nlp4s.base.LinkTag
import nlp4s.base.Mode
import nlp4s.base.NlpResult
import nlp4s.base.Tense
import nlp4s.english.lexicon.EnglishLinkTags
import nlp4s.english.lexicon.EnglishWordTags
import nlp4s.mrs.GraphInterpreter
import nlp4s.mrs.Handle
import nlp4s.mrs.InterpreterError
import nlp4s.mrs.MRS
import nlp4s.mrs.Relation
import nlp4s.mrs.Variable
import nlp4s.parser.Parser

class EnglishGraphInterpreter extends GraphInterpreter {
  import cats.syntax.all._
  import EnglishLinkTags._
  import EnglishWordTags._

  type Label = String

  def getLabel(position: Int): Interpret[Label] =
    collectFirstTag(position) { case EnglishWordTags.Label(l) => l }

  def verbTense(w: Int): Interpret[Tense] =
    collectFirstTag(w) { case EnglishWordTags.WordTense(tense) => tense }

  def verbRoot(w: Int): Interpret[String] =
    collectFirstTag(w) { case EnglishWordTags.VerbRoot(root) => root }

  def nounRoot(w: Int): Interpret[String] =
    collectFirstTag(w) { case EnglishWordTags.NounRoot(root) => root }

  def simpleTense(w: Int): Interpret[(Mode, Tense, Label)] = {
    for {
      _ <- guardEmpty(graphEdgeFrom(EnglishLinkTags.T, w))
      _ <- guardEmpty(graphEdgeFrom(EnglishLinkTags.H, w))
      _ <- graphEdgeFrom(EnglishLinkTags.S, w) <+> graphEdgeFrom(EnglishLinkTags.Qs, w)
      tense <- verbTense(w)
      label <- getLabel(w)
    } yield (Mode.Declarative, tense, label)
  }
    
  def imperativeTense(w: Int): Interpret[(Mode, Tense, Label)] = {
    for {
      _ <- guardEmpty(graphEdgeFrom(EnglishLinkTags.S, w))
      _ <- graphEdgeFrom(EnglishLinkTags.W, w)
      _ <- guardTokenHasTag(w, EnglishWordTags.RootForm)
      label <- getLabel(w)
    } yield (Mode.Imperative, Tense.Present, label)
  }

  def futureTense(w: Int): Interpret[(Mode, Tense, Label)] = {
    for {
      h <- graphEdgeFrom(EnglishLinkTags.H, w)
      s <- graphEdgeFrom(EnglishLinkTags.S, w)
      r <- verbRoot(h)
      _ <- guard(r == "will")
      _ <- guardTokenHasTag(w, EnglishWordTags.RootForm)
      label <- getLabel(w)
    } yield (if(h < s) Mode.Interrogative else Mode.Declarative, Tense.Future, label)
  }

  def progressiveTense(w: Int): Interpret[(Mode, Tense, Label)] = {
    for {
      h <- graphEdgeFrom(EnglishLinkTags.T, w)
      s <- graphEdgeFrom(EnglishLinkTags.S, w)
      r <- verbRoot(w)
      _ <- guard(r == "be")
      _ <- guardTokenHasTag(h, EnglishWordTags.WordTense(Tense.PresentParticiple))
      t <- collectFirstTag(w) { 
        case EnglishWordTags.WordTense(Tense.Present) => Tense.PresentProgressive
        case EnglishWordTags.WordTense(Tense.Past) => Tense.PastProgressive
      }
      label <- getLabel(h)
    } yield (if(w < s) Mode.Interrogative else Mode.Declarative, t, label)
  }

  def futureProgressiveTense(w: Int): Interpret[(Mode, Tense, Label)] = {
    for {
      h <- graphEdgeFrom(EnglishLinkTags.T, w)
      s <- graphEdgeFrom(EnglishLinkTags.S, w)
      x <- graphEdgeFrom(EnglishLinkTags.H, w)
      xr <- verbRoot(x)
      wr <- verbRoot(w)
      _ <- guard(wr == "be")
      _ <- guard(xr == "will")
      _ <- guardTokenHasTag(w, EnglishWordTags.WordTense(Tense.PresentParticiple))
      _ <- guardTokenHasTag(h, EnglishWordTags.RootForm)
      _ <- guardTokenHasTag(x, EnglishWordTags.RootForm)
      label <- getLabel(h)
    } yield (if(x < s) Mode.Interrogative else Mode.Declarative, Tense.FutureProgressive, label)
  }

  def verbPhraseTense(w: Int): Interpret[(Mode, Tense, Label)] = {
    simpleTense(w) <+> 
    imperativeTense(w) <+> 
    futureTense(w) <+> 
    progressiveTense(w) <+> 
    futureProgressiveTense(w)
  }

  def negation(w: Int): Interpret[Boolean] = 
    (graphEdgeFrom(EnglishLinkTags.N, w) >> pure(true)) <+> pure(false)

  type BuildVerbRelation = 
    (Variable, Option[Variable], Option[Variable]) => Interpret[Handle]

  def verb(w: Int): Interpret[BuildVerbRelation] = {
    for {
      t <- verbPhraseTense(w)
      mode = t._1
      tense = t._2
      label = t._3
      n <- negation(w)
    } yield {
      /*
      case (None, Some(obj), Some(biObj)) if mode == Mode.Imperative => {
        for {
          v <- makeVariable() // TODO add relation on v - imperative
          u <- makeGlobalVariable()
          _ <- addGlobalRelation(Relation.VerbMode(mode, u))
          _ <- addGlobalRelation(Relation.VerbTense(tense, u))
          h <- makeHandle()
          _ <- addRelation(h, Relation.BitransitiveVerb(u, label, v, obj, biObj))
        } yield h
      }
      case (None, Some(obj), None) if mode == Mode.Imperative => {
        for {
          v <- makeVariable() // TODO add relation on v - imperative
          u <- makeGlobalVariable()
          _ <- addGlobalRelation(Relation.VerbMode(mode, u))
          _ <- addGlobalRelation(Relation.VerbTense(tense, u))
          h <- makeHandle()
          _ <- addRelation(h, Relation.TransitiveVerb(u, label, v, obj))
        } yield h 
      }
      case (None, None, _) if mode == Mode.Imperative => {
        for {
          v <- makeVariable() // TODO add relation on v - imperative
          u <- makeGlobalVariable()
          _ <- addGlobalRelation(Relation.VerbMode(mode, u))
          _ <- addGlobalRelation(Relation.VerbTense(tense, u))
          h <- makeHandle()
          _ <- addRelation(h, Relation.IntransitiveVerb(u, label, v))
        } yield h
      }
      */
      case (subj, Some(obj), Some(biObj)) => {
        for {
          u <- makeGlobalVariable()
          _ <- addGlobalRelation(Relation.VerbMode(mode, u))
          _ <- addGlobalRelation(Relation.VerbTense(tense, u))
          h <- makeHandle()
          _ <- addRelation(h, Relation.BitransitiveVerb(u, label, subj, obj, biObj))
        } yield h
      }
      case (subj, Some(obj), None) => {
        for {
          u <- makeGlobalVariable()
          _ <- addGlobalRelation(Relation.VerbMode(mode, u))
          _ <- addGlobalRelation(Relation.VerbTense(tense, u))
          h <- makeHandle()
          _ <- addRelation(h, Relation.TransitiveVerb(u, label, subj, obj))
        } yield h
      }
      case (subj, None, _) => {
        for {
          u <- makeGlobalVariable()
          _ <- addGlobalRelation(Relation.VerbMode(mode, u))
          _ <- addGlobalRelation(Relation.VerbTense(tense, u))
          h <- makeHandle()
          _ <- addRelation(h, Relation.IntransitiveVerb(u, label, subj))
        } yield h
      }
      case _ => fail() 
    }
  }

  def implicitNounPhrase(): Interpret[(Handle, Handle, Variable)] = {
    for {
      h0 <- makeHandle()
      h1 <- makeHandle()
      h2 <- makeHandle()
      v  <- makeVariable()
      _  <- addRelation(h0, Relation.Implicit(v))
      _  <- addRelation(h1, Relation.Quantifier("implicit", v, h0, h2))
    } yield (h2, h1, v)
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
      obj <- toOption { 
        nounPhraseFrom(EnglishLinkTags.O, mainVerb) <+>
        (  
          graphEdgeFrom(EnglishLinkTags.T, mainVerb) >>= 
          (x => nounPhraseFrom(EnglishLinkTags.O, x))
        )
      }
      subj2 <- fromOption(subj) <+> implicitNounPhrase()
      handle <- vf(subj2._3, obj.map(_._3), None)
      top <- getMRSTop()
      _ <- addConstraint(subj2._1, handle)
      _ <- addConstraint(top, subj2._2)
      _ <- optional(obj.map { o => 
        addConstraint(o._1, handle) andThen addConstraint(top, o._2) 
      })
    } yield ()
  }

  type MakeQuantifier = (Handle, Handle) => Interpret[(Handle, Variable)]

  def determinerFrom(w: Int): Interpret[MakeQuantifier] = {
    for {
      d <- graphEdgeFrom(EnglishLinkTags.D, w)
      label <- getLabel(d)
      v <- makeVariable()
    } yield { (h1, h2) =>
      for {
        h0 <- makeHandle()
        _ <- addRelation(h0, Relation.Quantifier(label, v, h1, h2))
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
      npRel <- getLabel(w)
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

  def pronounPhrase(w: Int): Interpret[(Handle, Handle, Variable)] = {
    for {
      _ <- guardTokenHasTag(w, EnglishWordTags.Pronoun)
      person <- collectFirstTag(w) { case EnglishWordTags.Person(p) => p } 
      plural <- tokenHasTag(w, EnglishWordTags.Plural)
      gender <- collectTag(w) { case EnglishWordTags.Gender(g) => g } .map { _.headOption }
      v <- makeVariable()
      h0 <- makeHandle()
      _ <- addRelation(h0, Relation.Pronoun(person, plural, gender, v))
      h1 <- makeHandle()
      h2 <- makeHandle()
      _ <- addRelation(h1, Relation.Quantifier("implicit", v, h0, h2))
    } yield (h2, h1, v)
  }

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

