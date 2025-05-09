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

  def infinitiveTense(w: Int): Interpret[(Mode, Tense, Label)] = {
    for {
      _ <- graphEdgeFrom(EnglishLinkTags.S, w) <+> graphEdgeFrom(EnglishLinkTags.Qs, w)
      _ <- guardTokenHasTag(w, EnglishWordTags.RootForm)
      _ <- guardTokenHasTag(w, EnglishWordTags.Verb)
      label <- getLabel(w)
    } yield (Mode.Declarative, Tense.BareInfinitive, label)
  }

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
    futureProgressiveTense(w) <+>
    infinitiveTense(w)
  }

  def negation(w: Int): Interpret[Boolean] = 
    (graphEdgeFrom(EnglishLinkTags.N, w) >> pure(true)) <+> pure(false)

  def modalityFrom(w: Int, handle: Handle): Interpret[Handle] = {
    for {
      h <- graphEdgeFrom(EnglishLinkTags.H, w)
      s <- graphEdgeFrom(EnglishLinkTags.S, w)
      mode = if(h < s) Mode.Interrogative else Mode.Declarative
      r <- verbRoot(h)
      _ <- guard(r != "will") // tense (and 'will' for future) is treated separately
      n <- negation(h)
      tense <- verbTense(h)
      label <- getLabel(h)
      newHandle <- makeHandle()
      modalVariable <- makeVariable()
      _ <- addRelation(handle, Relation.Modal(modalVariable, label, n, newHandle))
      _ <- addGlobalRelation(Relation.VerbTense(tense, modalVariable))
      _ <- addGlobalRelation(Relation.VerbMode(mode, modalVariable))
    } yield newHandle
  }

  def adverbsFrom(w: Int, v: Variable): Interpret[List[Relation[Handle]]] = {
    val adverbLeft = {
      for {
        a <- graphEdgeLeft(EnglishLinkTags.A, w)
        _ <- guardTokenHasTag(a, EnglishWordTags.Adverb)
        label <- collectFirstTag(a) { case EnglishWordTags.Label(l) => l }
      } yield Relation.Adverb[Handle](label, v)
    }

    val adverbRight = {
      for {
        a <- graphEdgeRight(EnglishLinkTags.A, w)
        _ <- guardTokenHasTag(a, EnglishWordTags.Adverb)
        label <- collectFirstTag(a) { case EnglishWordTags.Label(l) => l }
      } yield Relation.Adverb[Handle](label, v)
    }

    for {
      l <- toOption(adverbLeft)
      r <- toOption(adverbRight)
    } yield List(l, r).flatten
  }

  def directionsFrom(w: Int, v: Variable): Interpret[Relation[Handle]] = {
    for {
      d <- graphEdgeFrom(EnglishLinkTags.E, w)
      _ <- guardTokenHasTag(d, EnglishWordTags.Direction)
      label <- getLabel(d)
    } yield Relation.Adverb[Handle](label, v)
  }

  def verb(
    w: Int, 
    h: Handle, 
    subj: Variable, 
    obj: Option[Variable],
    biObj: Option[Variable],
  ): Interpret[Unit] = {
    for {
      t <- verbPhraseTense(w)
      mode = t._1
      tense = t._2
      label = t._3
      n <- negation(w)
      u <- makeGlobalVariable()
      _ <- addGlobalRelation(Relation.VerbMode(mode, u))
      _ <- addGlobalRelation(Relation.VerbTense(tense, u))
      prepositions <- prepositionsFrom(w, u, h)
      adverbs <- adverbsFrom(w, u)
      direction <- toOption(directionsFrom(w, u))
      rel <- { (subj, obj, biObj) match {
        case (s, Some(o), Some(bo)) => {
          pure[Relation[Handle]](Relation.BitransitiveVerb(u, label, s, o, bo))
        }
        case (s, Some(o), None) => {
          pure[Relation[Handle]](Relation.TransitiveVerb(u, label, s, o))
        }
        case (s, None, None) => {
          pure[Relation[Handle]](Relation.IntransitiveVerb(u, label, s))
        }
        case _ => fail[Relation[Handle]]()
      } }
      relations = rel :: prepositions ++ adverbs ++ direction.toList
      h2 <- modalityFrom(w, h) <+> pure(h)
      _ <- addRelationBag(h2, relations)
    } yield ()
  }

  def implicitNounPhrase(h: Handle): Interpret[Variable] = {
    for {
      h0 <- makeHandle()
      h1 <- makeHandle()
      h2 <- makeHandle()
      v  <- makeVariable()
      _  <- addRelation(h0, Relation.Implicit(v))
      _  <- addRelation(h1, Relation.Quantifier("implicit", v, h0, h2))
      top <- getTop()
      _ <- addConstraint(top, h1)
      _ <- addConstraint(h2, h)
    } yield v
  }

  // TODO - support for bi-transitive verbs (parse second object)

  def indirectNounPhraseFrom(mainVerb: Int, vpHandle: Handle): Interpret[Variable] = {
    graphEdgeFrom(EnglishLinkTags.T, mainVerb) >>=
    { x => nounPhraseFrom(EnglishLinkTags.O, x, vpHandle) }
  }

  def verbPhrase(mainVerb: Int): Interpret[Unit] = {
    for {
      _ <- guardTokenHasTag(mainVerb, Verb)
      vpHandle <- makeHandle()
      subj <- nounPhraseFrom(EnglishLinkTags.S, mainVerb, vpHandle) <+> implicitNounPhrase(vpHandle)
      obj <- toOption {
        nounPhraseFrom(EnglishLinkTags.O, mainVerb, vpHandle) <+> 
        indirectNounPhraseFrom(mainVerb, vpHandle)
      }
      vp <- verb(mainVerb, vpHandle, subj, obj, None) // TODO bi-object
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

  def walkAdjectives(w: Int): Interpret[List[Int]] = {
    (graphEdgeLeft(EnglishLinkTags.J, w) >>= { x =>
      walkAdjectives(x) >>= { y => pure(x :: y) }
    }) <+> pure(List.empty)
  }

  def adjectivesFrom(w: Int, v: Variable): Interpret[List[Relation[Handle]]] = {
    for {
      adjPos <- walkAdjectives(w).map(_.reverse)
      relations <- adjPos.map(collectFirstTag(_) { 
        case EnglishWordTags.AdjectiveRoot(label) => Relation.Adjective[Handle](label, v)
      } ).sequence
    } yield relations
  }

  type MakePreposition = Variable => Interpret[Relation[Handle]]

  def walkPrepositions(w: Int): Interpret[List[Int]] = {
    (graphEdgeRight(EnglishLinkTags.P, w) >>= { x =>
      walkPrepositions(x) >>= { y => pure(x :: y) }
    }) <+> pure(List.empty)
  }

  def makePreposition(w: Int, v: Variable, h: Handle): Interpret[Relation[Handle]] = {
    for {
      _ <- guardTokenHasTag(w, EnglishWordTags.Preposition)
      ppLabel <- collectFirstTag(w) { case EnglishWordTags.Label(label) => label }
      n <- graphEdgeFrom(EnglishLinkTags.R, w)
      npVar <- nounPhrase(n, h)
    } yield Relation.Preposition(ppLabel, v, npVar)
  }

  def prepositionsFrom(w: Int, v: Variable, h: Handle): Interpret[List[Relation[Handle]]] = {
    for {
      ppPos <- walkPrepositions(w)
      relations <- ppPos.map(makePreposition(_, v, h)).sequence
    } yield relations
  }

  def countNounPhrase(w: Int, h: Handle): Interpret[Variable] = {
    for {
      _ <- guardTokenHasTag(w, EnglishWordTags.CountNoun)
      npRel <- getLabel(w)
      makeQuantifier <- determinerFrom(w)
      qh1 <- makeHandle()
      qh2 <- makeHandle()
      p <- makeQuantifier(qh1, qh2)
      (quantifierHandle, quantifierVariable) = p
      rh1 <- makeHandle()
      adjectives <- adjectivesFrom(w, quantifierVariable)
      prepositions <- prepositionsFrom(w, quantifierVariable, rh1)
      r1 = Relation.CountNoun[Handle](npRel, quantifierVariable) :: (adjectives ++ prepositions)
      _ <- addRelationBag(rh1, r1)
      _ <- addConstraint(qh1, rh1)
      top <- getTop()
      _ <- addConstraint(top, quantifierHandle)
      _ <- addConstraint(qh2, h)
    } yield quantifierVariable
  }

  def pronounPhrase(w: Int, h: Handle): Interpret[Variable] = {
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
      top <- getTop()
      _ <- addConstraint(top, h1)
      _ <- addConstraint(h2, h)
    } yield v
  }

  def massNounPhrase(w: Int, h: Handle): Interpret[Variable] =
    fail() // TODO

  def nounPhrase(w: Int, h: Handle): Interpret[Variable] = {
    countNounPhrase(w, h) <+> pronounPhrase(w, h) <+> massNounPhrase(w, h)
  }

  def nounPhraseFrom(tag: LinkTag, verb: Int, handle: Handle): Interpret[Variable] = {
    graphEdgeFrom(tag, verb) >>= { x => nounPhrase(x, handle) }
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

