package nlp4s.mrs

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
  def isQuantifier: Boolean = false
  def isVerb: Boolean = false
  def subject: Option[Variable] = None

  def mapH[I](f: H => I): Relation[I] 
  private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]]
}

object Relation {
  case class Recursive(handle: Handle, relations: List[Relation[Recursive]])

  case class Quantifier[H](
    quantifierName: String,
    variable: Variable,
    variablePredicate: H,
    scope: H
  ) extends Relation[H](quantifierName, List(variable), List.empty, List(variablePredicate, scope)) {
    override def isQuantifier = true

    def mapH[I](f: H => I): Relation[I] = 
      Quantifier(quantifierName, variable, f(variablePredicate), f(scope))

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] = {
      MRS.withVariables(List(variable)) {
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

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] =
      MRS.pure(CountNoun(nounName, variable))
  }

  case class Adjective[H](
    adjectiveName: String,
    variable: Variable
  ) extends Relation[H](adjectiveName, List.empty, List(variable), List.empty) {
    override def subject = Some(variable)

    def mapH[I](f: H => I): Relation[I] =
      Adjective(adjectiveName, variable)

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] =
      MRS.pure(Adjective(adjectiveName, variable))
  }

  case class AdjectiveRelation[H](
    adjectiveName: String,
    variable1: Variable,
    variable2: Variable
  ) extends Relation[H](adjectiveName, List.empty, List(variable1, variable2), List.empty) {
    def mapH[I](f: H => I): Relation[I] =
      AdjectiveRelation(adjectiveName, variable1, variable2)

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] =
      MRS.pure(AdjectiveRelation(adjectiveName, variable1, variable2))
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

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] = 
      MRS.pure(Preposition(prepositionName, arg0, arg1))
  }

  case class ScopalAdverb[H](
    adverbName: String,
    scope: H
  ) extends Relation[H](adverbName, List.empty, List.empty, List(scope)) {
    def mapH[I](f: H => I): Relation[I] =
      ScopalAdverb(adverbName, f(scope))

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] =
      for(s <- f(scope)) yield ScopalAdverb(adverbName, s)
  }

  trait VerbRelation[H] { self: Relation[H] => 
    abstract override def isVerb = true
  }

  case class IntransitiveVerb[H](
    verbName: String,
    arg0: Variable
  ) extends Relation[H](verbName, List.empty, List(arg0), List.empty) with VerbRelation[H] {
    override def subject = Some(arg0)

    def mapH[I](f: H => I): Relation[I] =
      IntransitiveVerb(verbName, arg0)

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] =
      MRS.pure(IntransitiveVerb(verbName, arg0))
  }

  case class TransitiveVerb[H](
    verbName: String,
    arg0: Variable,
    arg1: Variable
  ) extends Relation[H](verbName, List.empty, List(arg0, arg1), List.empty) with VerbRelation[H] {
    override def subject = Some(arg0)

    def mapH[I](f: H => I): Relation[I] =
      TransitiveVerb(verbName, arg0, arg1)

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] =
      MRS.pure(TransitiveVerb(verbName, arg0, arg1))
  }

  case class BitransitiveVerb[H](
    verbName: String,
    arg0: Variable,
    arg1: Variable,
    arg2: Variable
  ) extends Relation[H](verbName, List.empty, List(arg0, arg1, arg2), List.empty) with VerbRelation[H] {
    override def subject = Some(arg0)

    def mapH[I](f: H => I): Relation[I] =
      BitransitiveVerb(verbName, arg0, arg1, arg2)

    private[mrs] def flatMapH[I](f: H => MRS.F[I]): MRS.F[Relation[I]] =
      MRS.pure(BitransitiveVerb(verbName, arg0, arg1, arg2))
  }
}
