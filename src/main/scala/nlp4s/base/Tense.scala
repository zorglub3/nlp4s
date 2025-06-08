package nlp4s.base

sealed trait Tense {
  def asString(): String
}

object Tense {
  case object Present extends Tense {
    def asString() = "present"
  }

  case object Past extends Tense {
    def asString() = "past"
  }

  /*
  case object Future extends Tense {
    def asString() = "future"
  }
  */

  case object PresentProgressive extends Tense {
    def asString() = "present_progressive"
  }

  case object PastProgressive extends Tense {
    def asString() = "past_progressive"
  }

  /*
  case object FutureProgressive extends Tense {
    def asString() = "future_progressive"
  }
  */

  case object PresentPerfect extends Tense {
    def asString() = "present_perfect"
  }

  case object PastPerfect extends Tense {
    def asString() = "past_perfect"
  }

  /*
  case object FuturePerfect extends Tense {
    def asString() = "future_perfect"
  }
  */


  // These are not tenses but verb forms, but sometimes this is as
  // much as we know about the tense, eg, when tagging words.
  case object PresentParticiple extends Tense {
    def asString() = ""
  }

  case object PastParticiple extends Tense  {
    def asString() = ""
  }

  // example: "I want _to win_" (note the "to")
  case object FullInfinitive extends Tense {
    def asString() = "full_infinitive"
  }

  // example: "I can _do_ this"
  case object BareInfinitive extends Tense {
    def asString() = "infinitive"
  }
}
