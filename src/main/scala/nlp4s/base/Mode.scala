package nlp4s.base

sealed trait Mode {
  def asString(): String
}

object Mode {
  case object Declarative extends Mode {
    def asString() = "declarative"
  }

  case object Imperative extends Mode {
    def asString() = "imperative"
  }

  case object Interrogative extends Mode {
    def asString() = "interrogative"
  }

  case object Exclamatory extends Mode {
    def asString() = "exclamatory"
  }
}

