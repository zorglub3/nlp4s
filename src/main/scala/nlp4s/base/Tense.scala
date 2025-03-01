package nlp4s.base

sealed trait Tense

object Tense {
  case object Present extends Tense
  case object Past extends Tense
  case object Future extends Tense
  case object PresentProgressive extends Tense
  case object PastProgressive extends Tense
  case object FutureProgressive extends Tense
  case object PresentPerfect extends Tense
  case object PastPerfect extends Tense
  case object FuturePerfect extends Tense

  // These are not tenses but verb forms, but sometimes this is as
  // much as we know about the tense, eg, when tagging words.
  case object PresentParticiple extends Tense
  case object PastParticiple extends Tense 
}
