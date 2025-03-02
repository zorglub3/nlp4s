package nlp4s.english.std

import nlp4s.base.Gender
import nlp4s.base.Person
import nlp4s.english.Lexicon
import nlp4s.english.lexicon._

trait StandardWords { self: Lexicon.Builder =>
  addEntries(List(
    Wall,

    Negation(),
    
    PersonalPronoun("i", "me", "mine", Person.First, false, None),
    PersonalPronoun("you", "you", "yours", Person.Second, false, None),
    PersonalPronoun("he", "him", "his", Person.Third, false, Some(Gender.Masculine)),
    PersonalPronoun("she", "her", "hers", Person.Third, false, Some(Gender.Feminine)),
    PersonalPronoun("it", "it", "its", Person.Third, false, Some(Gender.Neuter)),
    PersonalPronoun("we", "us", "ours", Person.First, true, None),
    PersonalPronoun("you", "you", "yours", Person.Second, true, None),
    PersonalPronoun("they", "them", "theirs", Person.Third, true, None),

    DemonstrativePronoun("this", "these", true),
    DemonstrativePronoun("that", "those", false),

    ReflexivePronoun("myself", Person.First, false, None),
    ReflexivePronoun("yourself", Person.Second, false, None),
    ReflexivePronoun("himself", Person.Third, false, Some(Gender.Masculine)),
    ReflexivePronoun("herself", Person.Third, false, Some(Gender.Feminine)),
    ReflexivePronoun("itself", Person.Third, false, Some(Gender.Neuter)),
    ReflexivePronoun("ourselves", Person.First, true, None),
    ReflexivePronoun("yourselves", Person.Second, true, None),
    ReflexivePronoun("themselves", Person.Third, true, None),
    
    Determiner("the", false),
    Determiner("the", true),
    Determiner("a", false),
    Determiner("an", false),
    Determiner("many", true),
    Determiner("all", true),
    Determiner("all_the", true),
    Determiner("some", true),
    Determiner("some", false),
    Determiner("what", false),
    Determiner("what", true),
    Determiner("whoose", false),
    Determiner("whoose", true),
    Determiner("which", false),
    Determiner("each", false),
    Determiner("either", false),
    Determiner("neither", false),
    Determiner("both", true),
    Determiner("any", false),
    Determiner("few", true),
    Determiner("dozen", true),
    Determiner("a_dozen", true),
    Determiner("this", false),
    Determiner("that", false),
    Determiner("those", true),
    Determiner("these", true),
    Determiner("another", false),
    
    PossessiveDeterminer("my", Person.First, false),
    PossessiveDeterminer("your", Person.Second, false),
    PossessiveDeterminer("his", Person.Third, false),
    PossessiveDeterminer("her", Person.Third, false),
    PossessiveDeterminer("its", Person.Third, false),
    PossessiveDeterminer("our", Person.First, true),
    PossessiveDeterminer("your", Person.Second, true),
    PossessiveDeterminer("their", Person.Third, true),

    /*
    Preposition("to"),
    Preposition("from"),
    Preposition("over"),
    Preposition("under"),
    Preposition("with"),
    Preposition("on"),
    Preposition("like"),
    */

    Question(),
  ))

  val abbreviatoins = Map(
    "isnt" -> List("is", "not"),
    "doesnt" -> List("does", "not"),
    "didnt" -> List("did", "not"),
    "dont" -> List("do", "not"),
    "arent" -> List("are", "not"),
    "im" -> List("i", "am"),
    "havent" -> List("have", "not"),
    "hasnt" -> List("has", "not"))
}
