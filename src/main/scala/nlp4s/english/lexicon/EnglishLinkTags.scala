package nlp4s.english.lexicon

import nlp4s.base.LinkTag

object EnglishLinkTags {
  // S*: np-vp bindings
  case object S extends LinkTag("S")
  case object Ss extends LinkTag("Ss") // singular noun
  case object Sp extends LinkTag("Sp") // plural
  case object Spi extends LinkTag("Spi") // first person singular
  case object Spp extends LinkTag("Spp") // pluralis for eg "you are", "they are" - to avoid "I are"
  // case class Sq(suffix: String) extends LinkTag("Sq" + suffix) // for questions with 'to-be', eg, "who am i"
  case object Sq extends LinkTag("Sq")
  case object Sqs extends LinkTag("Sqs")
  case object Sqp extends LinkTag("Sqp")
  case object Sqpi extends LinkTag("Sqpi")
  case object Sqpp extends LinkTag("Sqpp")

  // O: verb-object binding
  case object O extends LinkTag("O")
  // D*: determiner-noun binding
  case object Ds extends LinkTag("Ds") // singular: a, an, every, one, any
  case object Dp extends LinkTag("Dp") // plural: all, many
  case object D  extends LinkTag("D")  // singular/plural: the, some
  // A: verb-adverb binding (includes direction)
  case object A extends LinkTag("A")
  // C: degree_adverb-adjective
  case object C extends LinkTag("C")
  // H: helpverb-verb binding (suffix has same meaning as for S- tags)
  case object H extends LinkTag("H")
  case object Hs extends LinkTag("Hs")
  case object Hp extends LinkTag("Hp")
  case object Hr extends LinkTag("Hr")
  case object Hpi extends LinkTag("Hpi")
  case object Hpp extends LinkTag("Hpp")
  case class Hq(suffix: String) extends LinkTag("Hq" + suffix)
  // T: To be binding Tr is p_r_esent participle, Ta is p_a_st participle
  case object T extends LinkTag("T")
  case object Tr extends LinkTag("Tr")
  case object Ta extends LinkTag("Ta")

  // Qw question words (where, why, et.c.)
  case object Qw extends LinkTag("Qw")
  // Qo question object (eg "who are you", "what did you take")
  case object Qo extends LinkTag("Qo")
  // Qs question subject (eg "who took the table")
  case object Qs extends LinkTag("Qs")
  // J adjective-noun binding
  case object J extends LinkTag("J")
  // P noun-preposition binding or (link-)verb-preposition binding
  case object P extends LinkTag("P")
  // R Preposition-noun eg "from the hills"
  case object R extends LinkTag("R")
  // N negation, ie, "not"
  case object N extends LinkTag("N")
  // W link to the wall
  case object W extends LinkTag("W")
  case object Wn extends LinkTag("Wn") // link to noun for noun-phrases
  case object Wv extends LinkTag("Wv") // link to verb for commands
  case object WW extends LinkTag("WW") // link to the other wall
  // B to-be/link-verb-adjective binding
  case object B extends LinkTag("B")
}

