package nlp4s.base

sealed trait Casus

object Casus {
  case object Nominative extends Casus
  case object Accusative extends Casus
  case object Possessive extends Casus

  // case object Genetive extends Casus
  // case object Dative extends Casus
}
