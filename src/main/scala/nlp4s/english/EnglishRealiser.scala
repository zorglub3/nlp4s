package nlp4s.english

import nlp4s.base.AdjectiveForm
import nlp4s.base.Casus
import nlp4s.mrs.Relation
import nlp4s.mrs.Variable
import nlp4s.realiser.Realiser

class EnglishRealiser(wordBook: WordBook) extends Realiser {
  import Relation._
  import cats.syntax.all._

  def allVerbs(rels: List[Relation[Recursive]]): Boolean =
    rels.forall { _.isVerb }

  case class NounPhrase(
    noun: Noun[Recursive],
    adjectives: List[Adjective[Recursive]],
    prepositions: List[Preposition[Recursive]]
  )

  def tellNounPhrase(v: Variable, casus: Casus): F[Unit] = {
    for {
      rels <- variableRelations(v)
      adjectives = rels.collect { case a: Adjective[_] => a } .toList
      nouns = rels.collect { case n: Noun[_] => n } .toList
      quantifier = rels.collect { case q: Quantifier[_] => q } .toList
      adjWords = adjectives.flatMap(a => wordBook.adjectiveForm(a.name, AdjectiveForm.Absolute))
      nounWords = nouns.flatMap(n => wordBook.nounForm(n.name, false, false))
      quantWords = quantifier.flatMap(q => wordBook.quantifierForm(q.name))
      _ <- tellMore(quantWords)
      _ <- tellMore(adjWords)
      _ <- tellMore(nounWords)
    } yield ()
  }

  def tellVerb(label: String): F[Unit] = {
    ???
  }

  def tellVerbPhrase(rel: Relation[Recursive]): F[Unit] = {
    rel match {
      case IntransitiveVerb(label, arg0) => {
        tellNounPhrase(arg0, Casus.Nominative) >> tellVerb(label)
      }
      case TransitiveVerb(label, arg0, arg1) => {
        tellNounPhrase(arg0, Casus.Nominative) >> tellVerb(label) >> tellNounPhrase(arg1, Casus.Accusative)
      }
      case BitransitiveVerb(label, arg0, arg1, arg2) => {
        tellNounPhrase(arg0, Casus.Nominative) >> tellVerb(label) >> tellNounPhrase(arg1, Casus.Accusative) >> tellNounPhrase(arg2, Casus.Accusative)
      }
      case _ => fail
    }
  }

  def tellRelationList(body: List[Relation[Recursive]]): F[Unit] = {
    body match {
      case List(IntransitiveVerb(label, arg0)) => ???
      case List(TransitiveVerb(label, arg0, arg1)) => ???
      case List(BitransitiveVerb(label, arg0, arg1, arg2)) => ???
      case _ => pure(())
    }
  }
}
