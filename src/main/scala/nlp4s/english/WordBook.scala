package nlp4s.english

import nlp4s.base.AdjectiveForm
import nlp4s.base.Person
import nlp4s.base.Tense
import nlp4s.english.lexicon._

// TODO modal verbs
case class WordBook(
  verbs: Map[String, WordBook.Entry.VerbEntry],
  nouns: Map[String, WordBook.Entry.NounEntry],
  adjectives: Map[String, WordBook.Entry.AdjectiveEntry],
  quantifiers: Map[String, WordBook.Entry.QuantifierEntry],
  prepositions: Map[String, WordBook.Entry.PrepositionEntry]
) {
  def toBeForm(
    person: Person,
    plural: Boolean,
    tense: Tense,
  ): String = {
    (person, plural, tense) match {
      case (Person.First, false, Tense.Present) => "am"
      case (Person.Third, false, Tense.Present) => "is"
      case (_, _, Tense.Present) => "are"
      case (Person.First, false, Tense.Past) => "was"
      case (Person.Third, false, Tense.Past) => "was"
      case (_, _, Tense.Past) => "were"
      case (_, _, Tense.BareInfinitive) => "be"
      case (_, _, Tense.FullInfinitive) => "be"
      case (_, _, Tense.PresentParticiple) => "being"
      case (_, _, Tense.PastParticiple) => "been"
      case _ => "be"
    }
  }

  def toHaveForm(
    person: Person,
    plural: Boolean,
    tense: Tense,
  ): String = {
    ???
  }

  def verbForm(
    label: String, 
    person: Person, 
    plural: Boolean, 
    tense: Tense
  ): Option[String] = {
    import WordBook.Entry._

    if(label == "be") {
      Some(toBeForm(person, plural, tense))
    } else {
      verbs.get(label).flatMap {
        case VerbEntry(r, ps, pp, ppart, past, pastpart) => {
          (person, plural, tense) match {
            case (Person.Third, false, Tense.Present) => Some(ps)
            case (_, _, Tense.Present) => Some(pp)
            case (_, _, Tense.Past) => Some(past)
            case (_, _, Tense.BareInfinitive) => Some(r)
            case (_, _, Tense.PresentParticiple) => Some(ppart)
            case (_, _, Tense.PastParticiple) => Some(pastpart)
            case (_, _, Tense.FullInfinitive) => Some(r)
            case x => { println(s"AAARGH! $x") ; None } // TODO
          }
        }
        case _ => None
      }
    }
  }

  def prepositionForm(label: String): Option[String] = {
    prepositions.get(label).map(_.word)
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
    lazy val prepositionEntries = Map.newBuilder[String, Entry.PrepositionEntry]

    def addEntry(label: String, entry: EnglishLexiconEntry): Builder = {
      entry match {
        case IntransitiveVerb(r, ps, pp, ppart, past, pastpart) => verbEntries += label -> Entry.VerbEntry(r, ps, pp, ppart, past, pastpart) 
        case TransitiveVerb(r, ps, pp, ppart, past, pastpart) => verbEntries += label -> Entry.VerbEntry(r, ps, pp, ppart, past, pastpart)
        case HelpVerb(r, ps, pp, ppart, past, pastpart) => verbEntries += label -> Entry.VerbEntry(r, ps, pp, ppart, past, pastpart)
        case LinkVerb(r, ps, pp, ppart, past, pastpart) => verbEntries += label -> Entry.VerbEntry(r, ps, pp, ppart, past, pastpart)
        case ModalVerb(f, false, None) => verbEntries += label -> Entry.VerbEntry(f, f, f, f, f, f)
        case Noun(s, p, sp, pp) => nounEntries += label -> Entry.NounEntry(s, p, sp, pp, false)
        case MassNoun(s, sp) => nounEntries += label -> Entry.NounEntry(s, "", sp, "", true)
        case Adjective(a, c, s) => adjectiveEntries += label -> Entry.AdjectiveEntry(a, c, s, false) 
        case SimpleAdjective(a) => adjectiveEntries += label -> Entry.AdjectiveEntry(a, "", "", true)
        case Determiner(_label, w, plural) => quantifierEntries += label -> Entry.QuantifierEntry(w, plural)
        case Preposition(word) => prepositionEntries += word -> Entry.PrepositionEntry(word)
        case _ => {} // ignore for now
      }

      this
    }

    def result(): WordBook = {
      WordBook(
        verbEntries.result(),
        nounEntries.result(),
        adjectiveEntries.result(),
        quantifierEntries.result(),
        prepositionEntries.result()
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

    case class ModalVerbEntry(
      form: String,
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

    case class PrepositionEntry(
      word: String,
    )
  }
}
