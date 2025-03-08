package nlp4s.english

import nlp4s.realiser.MRSRealiser
import nlp4s.mrs.Relation
import nlp4s.mrs.Variable

class EnglishRealiser extends MRSRealiser {
  import cats.syntax.all._

  import Relation._

  def collectRelation(rel: Relation[Recursive]): F[Unit] = ???

  def collectRecursive(v: Variable, rec: Relation.Recursive): F[Unit] = {
    rec match {
      case Relation.Recursive(handle, relations) => relations.map(collectRelation).sequenceVoid
    }
  }

  def realiseRelation(rel: Relation[Recursive]): F[Unit] = {
    import Relation._

    rel match {
      case Quantifier(name, variable, varPred, scope) => {
        collectRecursive(variable, varPred) >> realiseRecursive(scope)
      }
      case CountNoun(name, variable) => pure(())
      case Adjective(name, variable) => pure(()) 
      case AdjectiveRelation(name, variable1, variable2) => pure(())
      case IntransitiveVerb(name, subject) => ???
      case TransitiveVerb(name, subject, obj) => ???
      case BitransitiveVerb(name, subject, obj, biobj) => ???
      case _ => fail 
    }
  }

  def realiseRecursive(rec: Relation.Recursive): F[Unit] = {
    rec match {
      case Relation.Recursive(handle, relations) => relations.map(realiseRelation).sequenceVoid
    }
  }
}
