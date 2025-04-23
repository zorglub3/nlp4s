package nlp4s.base

sealed trait Person {
  val asString: String
}

object Person {
  case object First extends Person {
    val asString = "first"
  }

  case object Second extends Person {
    val asString = "second"
  }

  case object Third extends Person {
    val asString = "third"
  }
}
