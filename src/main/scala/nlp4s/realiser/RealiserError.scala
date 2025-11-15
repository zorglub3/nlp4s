package nlp4s.realiser

import nlp4s.base.NlpError
import nlp4s.mrs.Variable
import nlp4s.mrs.Relation

class RealiserError(msg: String) extends NlpError(msg)

case class RealiserMissingTense(v: Variable) extends RealiserError(s"Verb (with variable: $v) is missing tense")
case class RealiserMissingMode(v: Variable) extends RealiserError(s"Verb (with variable: $v) is missing mode")
case class RealiserRelationFail(r: Relation[_]) extends RealiserError(s"Unable to realise relation: $r")
case class RealiserClauseFail(c: Clause) extends RealiserError(s"Unable to realise clause: $c")
case class RealiserMissingWord(w: String) extends RealiserError(s"Missing word $w")
case class RealiserMissingQuantifier(v: Variable) extends RealiserError(s"Missing quantifier for variable: $v")
case class RealiserMissingPronoun(v: Variable) extends RealiserError(s"Missing pronoun for variable: $v")
