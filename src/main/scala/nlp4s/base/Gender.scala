package nlp4s.base

sealed trait Gender

object Gender {
  case object Masculine extends Gender
  case object Feminine extends Gender
  case object Neuter extends Gender
}
