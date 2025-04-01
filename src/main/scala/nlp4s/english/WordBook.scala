package nlp4s.english

import nlp4s.base.AdjectiveForm
import nlp4s.base.Person
import nlp4s.base.Tense
import nlp4s.english.lexicon._

case class WordBook(
  verbs: Map[String, WordBook.Entry.VerbEntry],
  nouns: Map[String, WordBook.Entry.NounEntry],
  adjectives: Map[String, WordBook.Entry.AdjectiveEntry],
  quantifiers: Map[String, WordBook.Entry.QuantifierEntry]
) {
  def verbForm(
    label: String, 
    person: Person, 
    plural: Boolean, 
    tense: Tense
  ): Option[String] = {
    import WordBook.Entry._

    verbs.get(label).flatMap {
      case VerbEntry(r, ps, pp, ppart, past, pastpart) => {
        (person, plural, tense) match {
          case (Person.Third, false, Tense.Present) => Some(ps)
          case (_, _, Tense.Present) => Some(pp)
          case (_, _, Tense.Past) => Some(past)
          case _ => None // TODO
        }
      }
      case _ => None
    }
  }

  def nounForm(label: String, plural: Boolean, possessive: Boolean): Option[String] = {
    import WordBook.Entry._

    nouns.get(label).flatMap {
      case NounEntry(s, p, sp, pp, false) => {
        (plural, possessive) match {
          case (false, false) => Some(s)
          case (true, false) => Some(p)
          case (false, true) => Some(sp)
          case (true, true) => Some(pp)
        }
      }
      case NounEntry(s, p, sp, pp, true) => {
        (plural, possessive) match {
          case (true, false) => Some(s)
          case (true, true) => Some(sp)
          case _ => None
        }
      }
      case _ => None
    }
  }

  def adjectiveForm(label: String, form: AdjectiveForm): Option[String] = {
    import WordBook.Entry._
    import AdjectiveForm._

    adjectives.get(label).flatMap {
      case AdjectiveEntry(a, c, s, simple) => {
        form match {
          case Absolute => Some(a)
          case Comparative if !simple => Some(c)
          case Superlative if !simple => Some(s)
          case _ => None
        }
      }
    }
  }

  def quantifierForm(label: String): Option[String] = {
    quantifiers.get(label).map(_.word)
  }

  def quantifierPlural(label: String): Option[Boolean] = {
    quantifiers.get(label).map(_.plural)
  }
}

object WordBook {
  def newBuilder = new Builder

  class Builder {
    lazy val verbEntries = Map.newBuilder[String, Entry.VerbEntry]
    lazy val nounEntries = Map.newBuilder[String, Entry.NounEntry]
    lazy val adjectiveEntries = Map.newBuilder[String, Entry.AdjectiveEntry]
    lazy val quantifierEntries = Map.newBuilder[String, Entry.QuantifierEntry]

    def addEntry(label: String, entry: EnglishLexiconEntry): Builder = {
      entry match {
        case IntransitiveVerb(r, ps, pp, ppart, past, pastpart) => verbEntries += label -> Entry.VerbEntry(r, ps, pp, ppart, past, pastpart) 
        case TransitiveVerb(r, ps, pp, ppart, past, pastpart) => verbEntries += label -> Entry.VerbEntry(r, ps, pp, ppart, past, pastpart)
        case Noun(s, p, sp, pp) => nounEntries += label -> Entry.NounEntry(s, p, sp, pp, false)
        case MassNoun(s, sp) => nounEntries += label -> Entry.NounEntry(s, "", sp, "", true)
        case Adjective(a, c, s) => adjectiveEntries += label -> Entry.AdjectiveEntry(a, c, s, false) 
        case SimpleAdjective(a) => adjectiveEntries += label -> Entry.AdjectiveEntry(a, "", "", true)
        case Determiner(_label, w, plural) => quantifierEntries += label -> Entry.QuantifierEntry(w, plural)
        case _ => {} // ignore for now
      }

      this
    }

    def result(): WordBook = {
      WordBook(
        verbEntries.result(),
        nounEntries.result(),
        adjectiveEntries.result(),
        quantifierEntries.result()
      )
    }
  }

  sealed trait Entry

  object Entry {
    case class VerbEntry(
      root: String,
      presentSingular: String,
      presentPlural: String,
      presentParticiple: String,
      past: String,
      pastParticiple: String
    )

    case class NounEntry(
      singular: String,
      plural: String,
      singularPossessive: String,
      pluralPossessive: String,
      massNoun: Boolean
    )

    case class AdjectiveEntry(
      absolute: String,
      comparative: String,
      superlative: String,
      absoluteOnly: Boolean
    )

    case class QuantifierEntry(
      word: String,
      plural: Boolean
    )
  }
}
