package nlp4s.base

sealed trait AdjectiveForm

object AdjectiveForm {
  case object Absolute extends AdjectiveForm
  case object Comparative extends AdjectiveForm
  case object Superlative extends AdjectiveForm
}
