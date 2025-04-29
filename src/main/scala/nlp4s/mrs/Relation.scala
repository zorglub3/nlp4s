package nlp4s.mrs

import nlp4s.base.{Tense, Mode, Person, Gender}

abstract class Relation[H](
  /// String name of relation for lookup and interpretation
  val name: String,

  /// Variables that are scoped by this relation, ie, variables that can
  /// occur in the scopal arguments of this relation.
  val scopedVariables: Seq[Variable],

  /// Variables that used in this relation. They must be in scope.
  val variableArgs: Seq[Variable],

  /// Scopal arguments for the relation - the "holes" in the relation that
  /// link it to other relations.
  val scopalArgs: Seq[H]
) { 
  def allVariables: Seq[Variable] = scopedVariables ++ variableArgs
  def isQuantifier: Boolean = false
  def isVerb: Boolean = false
  def subject: Option[Variable] = None

  def mapH[I](f: H => I): Relation[I] 
  private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]]
}

object Relation {
  case class Recursive(handle: Handle, relations: List[Relation[Recursive]]) {
    def allHandles: List[Handle] = {
      val nested = 
        for {
          rel <- relations
          arg <- rel.scopalArgs
          han <- arg.allHandles
        } yield han

      handle :: nested 
    }
  }

  def pp(r: Recursive): Unit = {
    def pp_relation(p: Relation[Recursive]): String = {
      s"${p.name}(${p.allVariables.map(_.name).mkString(", ")})"
    }

    def pp_recursive(indent: Int, p: Recursive): Unit = {
      print("  " * indent)
      println(s"${p.handle.asString}: ${p.relations.map(pp_relation(_)).mkString(", ")}")
      for {
        rr <- p.relations
        scope <- rr.scopalArgs.toList
      } pp_recursive(indent + 1, scope)
    }

    pp_recursive(0, r)
  }

  case class Quantifier[H](
    quantifierName: String,
    variable: Variable,
    variablePredicate: H,
    scope: H
  ) extends Relation[H](quantifierName, List(variable), List.empty, List(variablePredicate, scope)) {
    override def isQuantifier = true

    def mapH[I](f: H => I): Relation[I] = 
      Quantifier(quantifierName, variable, f(variablePredicate), f(scope))

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] = {
      QuantifierScope.withVariables(List(variable)) {
        for {
          vp <- f(variablePredicate)
          s  <- f(scope)
        } yield Quantifier(quantifierName, variable, vp, s)
      }
    }
  }

  trait Noun[H] { self: Relation[H] => }

  case class CountNoun[H](
    nounName: String,
    variable: Variable
  ) extends Relation[H](nounName, List.empty, List(variable), List.empty) with Noun[H] {
    override def subject = Some(variable)

    def mapH[I](f: H => I): Relation[I] =
      CountNoun(nounName, variable)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(CountNoun(nounName, variable))
  }

  case class Pronoun[H](
    person: Person,
    plural: Boolean,
    gender: Option[Gender],
    variable: Variable
  ) extends Relation[H]("pronoun_" ++ person.asString ++ "_" ++ (if(plural) "plural" else "singular"), List.empty, List(variable), List.empty) {
    override def subject = Some(variable)

    def mapH[I](f: H => I): Relation[I] =
      Pronoun(person, plural, gender, variable)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(Pronoun(person, plural, gender, variable))
  }

  case class Adjective[H](
    adjectiveName: String,
    variable: Variable
  ) extends Relation[H](adjectiveName, List.empty, List(variable), List.empty) {
    override def subject = Some(variable)

    def mapH[I](f: H => I): Relation[I] =
      Adjective(adjectiveName, variable)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(Adjective(adjectiveName, variable))
  }

  case class AdjectiveRelation[H](
    adjectiveName: String,
    variable1: Variable,
    variable2: Variable
  ) extends Relation[H](adjectiveName, List.empty, List(variable1, variable2), List.empty) {
    def mapH[I](f: H => I): Relation[I] =
      AdjectiveRelation(adjectiveName, variable1, variable2)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(AdjectiveRelation(adjectiveName, variable1, variable2))
  }

  case class Preposition[H](
    prepositionName: String,
    arg0: Variable,
    arg1: Variable
  ) extends Relation[H](
    prepositionName,
    List.empty,
    List(arg0, arg1),
    List.empty
  ) {
    override def subject = Some(arg0)

    def mapH[I](f: H => I): Relation[I] = Preposition(prepositionName, arg0, arg1)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] = 
      QuantifierScope.pure(Preposition(prepositionName, arg0, arg1))
  }

  case class ScopalAdverb[H](
    adverbName: String,
    scope: H
  ) extends Relation[H](adverbName, List.empty, List.empty, List(scope)) {
    def mapH[I](f: H => I): Relation[I] =
      ScopalAdverb(adverbName, f(scope))

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      for(s <- f(scope)) yield ScopalAdverb(adverbName, s)
  }

  trait VerbRelation[H] { self: Relation[H] => 
    abstract override def isVerb = true
    def variable: Variable
    def args: List[Variable]
  }

  case class AuxVerb[H](
    variable: Variable,
    verbName: String,
    arg0: Variable,
    scope: H
  ) extends Relation[H](verbName, List.empty, List(variable, arg0), List(scope)) with VerbRelation[H] {
    def mapH[I](f: H => I): Relation[I] =
      AuxVerb(variable, verbName, arg0, f(scope))

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      for(s <- f(scope)) yield AuxVerb(variable, verbName, arg0, s)

    def args = List(arg0)
  }

  case class IntransitiveVerb[H](
    variable: Variable,
    verbName: String,
    arg0: Variable
  ) extends Relation[H](verbName, List.empty, List(variable, arg0), List.empty) with VerbRelation[H] {
    override def subject = Some(arg0)

    def mapH[I](f: H => I): Relation[I] =
      IntransitiveVerb(variable, verbName, arg0)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(IntransitiveVerb(variable, verbName, arg0))

    def args = List(arg0)
  }

  case class TransitiveVerb[H](
    variable: Variable,
    verbName: String,
    arg0: Variable,
    arg1: Variable
  ) extends Relation[H](verbName, List.empty, List(variable, arg0, arg1), List.empty) with VerbRelation[H] {
    override def subject = Some(arg0)

    def mapH[I](f: H => I): Relation[I] =
      TransitiveVerb(variable, verbName, arg0, arg1)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(TransitiveVerb(variable, verbName, arg0, arg1))

    def args = List(arg0, arg1)
  }

  case class BitransitiveVerb[H](
    variable: Variable,
    verbName: String,
    arg0: Variable,
    arg1: Variable,
    arg2: Variable
  ) extends Relation[H](verbName, List.empty, List(variable, arg0, arg1, arg2), List.empty) with VerbRelation[H] {
    override def subject = Some(arg0)

    def mapH[I](f: H => I): Relation[I] =
      BitransitiveVerb(variable, verbName, arg0, arg1, arg2)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(BitransitiveVerb(variable, verbName, arg0, arg1, arg2))

    def args = List(arg0, arg1, arg2)
  }

  case class VerbMode[H](
    mode: Mode,
    arg0: Variable
  ) extends Relation[H]("mode_" + mode.asString(), List.empty, List(arg0), List.empty) {
    def mapH[I](f: H => I): Relation[I] = VerbMode(mode, arg0)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(VerbMode(mode, arg0))
  }

  case class VerbTense[H](
    tense: Tense,
    arg0: Variable
  ) extends Relation[H]("tense_" + tense.asString(), List.empty, List(arg0), List.empty) {
    def mapH[I](f: H => I): Relation[I] = VerbTense(tense, arg0)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(VerbTense(tense, arg0))
  }

  case class Implicit[H](
    variable: Variable,
  ) extends Relation[H]("implicit", List.empty, List(variable), List.empty) {
    def mapH[I](f: H => I): Relation[I] = Implicit(variable)

    private[mrs] def flatMapH[I](f: H => QuantifierScope.F[I]): QuantifierScope.F[Relation[I]] =
      QuantifierScope.pure(Implicit(variable))
  }
}
