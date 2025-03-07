package nlp4s.base

sealed trait Mode

object Mode {
  case object Declarative extends Mode
  case object Imperative extends Mode
  case object Interrogative extends Mode
  case object Exclamatory extends Mode
}

