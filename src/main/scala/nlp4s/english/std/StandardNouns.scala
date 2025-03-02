package nlp4s.english.std

import nlp4s.english.Lexicon
import nlp4s.english.lexicon.MassNoun
import nlp4s.english.lexicon.Noun

trait StandardNouns { self: Lexicon.Builder =>
  addEntries(List(
    Noun("beer", "beers", "beers", "beers"),
    Noun("table", "tables", "tables", "tables"),
    Noun("chair", "chairs", "chairs", "chairs"),
    Noun("house", "houses", "houses", "houses"),
    Noun("road", "roads", "roads", "roads"),
    Noun("knife", "knives", "knifes", "knives"),
    Noun("fork", "forks", "forks", "forks"),
    Noun("plate", "plates", "plates", "plates"),
    Noun("man", "men", "mans", "mens"),
    Noun("woman", "women", "womans", "womens"),
    Noun("telescope", "telescopes", "telescopes", "telescopes"),
    Noun("drink", "drinks", "drinks", "drinks"),
    Noun("dog", "dogs", "dogs", "dogs"),
    Noun("cat", "cats", "cats", "cats"),
    Noun("leg", "legs", "legs", "legs"),
    Noun("room", "rooms", "rooms", "rooms"),
    Noun("sip", "sips", "sips", "sips"),
    Noun("glass", "glasses", "glass", "glasses"),
    Noun("bottle", "bottles", "bottles", "bottles"),
    MassNoun("water", "waters"),
    MassNoun("middle", "middles"),
  ))
}
