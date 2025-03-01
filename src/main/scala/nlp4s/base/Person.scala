package nlp4s.base

sealed trait Person

object Person {
  case object First extends Person
  case object Second extends Person
  case object Third extends Person
}
