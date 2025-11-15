package nlp4s.english

import nlp4s.base.AdjectiveForm
import nlp4s.base.Casus
import nlp4s.base.Mode
import nlp4s.base.Person
import nlp4s.base.Gender
import nlp4s.base.Tense
import nlp4s.mrs.Relation
import nlp4s.mrs.Variable
import nlp4s.realiser._
import cats.syntax.all._

// TODO cleanup comments + deal with modal verbs
class EnglishRealiser(wordBook: WordBook) extends Realiser {
  import Relation._

  def allVerbs(rels: List[Relation[Recursive]]): Boolean =
    rels.forall { _.isVerb }

  sealed trait NPGrammar

  case class ProperNP(
    person: Person,
    plural: Boolean,
  ) extends NPGrammar

  case object ImperativeNP extends NPGrammar

  def tellVerb(rel: String, grammar: NPGrammar, tense: Tense): F[Unit] = {
    grammar match {
      case ProperNP(person, plural) => {
        for {
          form <- liftOption(wordBook.verbForm(rel, person, plural, tense), RealiserMissingWord(rel))
          _ <- when(tense == Tense.FullInfinitive) { tell("to") } 
          _ <- tell(form)
        } yield ()
      }
      case ImperativeNP => {
        for {
          form <- liftOption(wordBook.verbForm(rel, Person.Second, false, Tense.BareInfinitive), RealiserMissingWord(rel))
          _ <- tell(form)
        } yield ()
      }
    }
  }

  // Deal with eg present progressive. It uses aux verb to-be with "tense"
  // present and main verb with tense "present participle", so 
  // tenseForms(Tense.PresentProgressive, interrogative = false|true) 
  //   ===> (Some(("be", Tense.Present)), Tense.PresentParticiple)
  def tenseForms(tense: Tense, interrogative: Boolean): (Option[(String, Tense)], Tense) = {
    import Tense._

    tense match {
      case Present if interrogative => (Some(("do", Present)), BareInfinitive)
      case Present => (None, Present)
      case Past if interrogative => (Some(("do", Past)), BareInfinitive)
      case Past => (None, Past)
      // case Future => (Some(("will", Present)), BareInfinitive)
      case PresentProgressive => (Some(("be", Present)), PresentParticiple)
      case PastProgressive => (Some(("be", Past)), PresentParticiple)
      // case FutureProgressive => ???
      case PresentPerfect => (Some(("be", Present)), PastParticiple)
      case PastPerfect => (Some(("have", Past)), PastParticiple)
      // case FuturePerfect => ???
      case PresentParticiple => (None, PresentParticiple)
      case PastParticiple => (None, PastParticiple)
      case FullInfinitive => (None, FullInfinitive)
      case BareInfinitive => (None, BareInfinitive)
    }
  }
      
  def tellNoun(rel: String, plural: Boolean, possessive: Boolean): F[Unit] = {
    for {
      form <- liftOption(wordBook.nounForm(rel, plural, possessive), RealiserMissingWord(rel))
      _ <- tell(form)
    } yield ()
  }

  def nounPhraseGrammar(v: Variable): F[NPGrammar] = {
    val properNP: F[NPGrammar] = 
      for {
        relations <- variableRelations(v)
        quantifier <- liftOption(relations.collectFirst { case q: Quantifier[_] => q }, RealiserMissingQuantifier(v))
        plural <- liftOption(wordBook.quantifierPlural(quantifier.name), RealiserMissingWord(quantifier.name))
      } yield ProperNP(Person.Third, plural)

    lazy val pronounNP: F[NPGrammar] = 
      for {
        relations <- variableRelations(v)
        pronoun <- liftOption(relations.collectFirst {
          case p: Pronoun[_] => p
        }, RealiserMissingPronoun(v))
      } yield ProperNP(pronoun.person, pronoun.plural)

    properNP <+> pronounNP
  }

  def tellProperNounPhrase(v: Variable, casus: Casus): F[Unit] = {
    for {
      rels <- variableRelations(v)
      quantifier <- liftOption(rels.collectFirst { case q: Quantifier[_] => q }, RealiserMissingQuantifier(v))
      quantWord <- liftOption(wordBook.quantifierForm(quantifier.name), RealiserMissingWord(quantifier.name))
      quantPlural <- liftOption(wordBook.quantifierPlural(quantifier.name), RealiserMissingWord(quantifier.name))
      adjectives = rels.collect { case a: Adjective[_] => a } .toList
      nouns = rels.collect { case n: Noun[_] => n } .toList
      adjWords = adjectives.flatMap(a => wordBook.adjectiveForm(a.name, AdjectiveForm.Absolute))
      nounWords = nouns.flatMap(n => wordBook.nounForm(n.name, quantPlural, false))
      _ <- tell(quantWord)
      _ <- tellMore(adjWords)
      _ <- tellMore(nounWords)
    } yield ()
  }

  def pronounWord(
    person: Person, 
    plural: Boolean, 
    gender: Option[Gender], 
    casus: Casus
  ): F[String] = {
    import Person._
    import Casus._
    import Gender._

    liftOption(
      (person, plural, gender, casus) match {
        case (First, false, _, Nominative) => Some("I")
        case (Second, false, _, Nominative) => Some("you")
        case (Third, false, Some(Masculine), Nominative) => Some("he")
        case (Third, false, Some(Feminine), Nominative) => Some("she")
        case (Third, false, _, Nominative) => Some("it")
        case (First, true, _, Nominative) => Some("we")
        case (Second, true, _, Nominative) => Some("you")
        case (Third, true, _, Nominative) => Some("they")
        case (First, false, _, Accusative) => Some("me")
        case (Second, false, _, Accusative) => Some("you")
        case (Third, false, Some(Masculine), Accusative) => Some("him")
        case (Third, false, Some(Feminine), Accusative) => Some("her")
        case (Third, false, _, Accusative) => Some("it")
        case (First, true, _, Accusative) => Some("us")
        case (Second, true, _, Accusative) => Some("you")
        case (Third, true, _, Accusative) => Some("them")
        case _ => None
      }, RealiserMissingWord((person, plural, gender, casus).toString())
    )
  }

  def tellPronounPhrase(v: Variable, casus: Casus): F[Unit] = {
    for {
      rels <- variableRelations(v) // TODO also global predicates
      pronoun <- liftOption(rels.collectFirst { case p: Pronoun[_] => p }, RealiserMissingPronoun(v))
      word <- pronounWord(pronoun.person, pronoun.plural, pronoun.gender, casus)
      _ <- tell(word)
    } yield ()
  }

  def tellNounPhrase(v: Variable, casus: Casus): F[Unit] = {
    tellProperNounPhrase(v, casus) <+> tellPronounPhrase(v, casus)
  }

  def tellImperative(rel: Relation[Recursive] with VerbRelation[_]): F[Unit] = {
    val args = rel.args.tail

    for {
      _ <- tellVerb(rel.name, ImperativeNP, Tense.BareInfinitive)
      _ <- args.map(tellNounPhrase(_, Casus.Accusative)).sequence
    } yield ()
  }

  def tellInterrogative(rel: Relation[Recursive] with VerbRelation[_]): F[Unit] = {
    val subject = rel.args.head
    val args = rel.args.tail

    for {
      tense <- verbTense(rel.variable)
      (auxForm, mainForm) = tenseForms(tense, true)
      grammar <- nounPhraseGrammar(subject)
      _ <- whenOpt(auxForm) { case (l, t) => tellVerb(l, grammar, t) }
      _ <- tellNounPhrase(subject, Casus.Nominative)
      _ <- tellVerb(rel.name, grammar, mainForm)
      _ <- args.map(tellNounPhrase(_, Casus.Accusative)).sequence
    } yield ()
  }

  def tellDeclarative(rel: Relation[Recursive] with VerbRelation[_]): F[Unit] = {
    val subject = rel.args.headOption.getOrElse(???)
    val args = rel.args.tail

    for {
      tense <- verbTense(rel.variable)
      (auxForm, mainForm) = tenseForms(tense, false)
      grammar <- nounPhraseGrammar(subject)
      _ <- tellNounPhrase(subject, Casus.Nominative)
      _ <- whenOpt(auxForm) { case (l, t) => tellVerb(l, grammar, t) }
      _ <- tellVerb(rel.name, grammar, mainForm)
      _ <- args.map(tellNounPhrase(_, Casus.Accusative)).sequence
    } yield ()
  }

  /*
  def tellVerbPhrase(rel: Relation[Recursive]): F[Unit] = {
    rel match {
      case vr: VerbRelation[_] => {
        verbMode(vr.variable) >>= {
          case Mode.Imperative => tellImperative(vr)
          case Mode.Interrogative => tellInterrogative(vr)
          case Mode.Declarative => tellDeclarative(vr)
          case Mode.Exclamatory => tellDeclarative(vr)
        }
      }
      case _ => fail
    }
  }
  */

 def tellSingleRelation(relation: Relation[Recursive], f: Recursive => F[Unit]): F[Unit] = {
    relation match {
      case Modal(v, modality, negated, scope) => {
        for {
          _ <- pushModality(modality, negated)
          _ <- f(scope)
          _ <- popModality()
        } yield ()
      }
      case vr: VerbRelation[_] => {
        verbMode(vr.variable) >>= {
          case Mode.Imperative => tellImperative(vr)
          case Mode.Interrogative => tellInterrogative(vr)
          case Mode.Declarative => tellDeclarative(vr)
          case Mode.Exclamatory => tellDeclarative(vr)
        }
      }
      case _ => failRelation(relation)
    }
  }
 
  def tellRelationList(body: List[Relation[Recursive]], f: Recursive => F[Unit]): F[Unit] = {
    body.map(tellSingleRelation(_, f)).sequenceVoid
  }
}
