package nlp4s.english.std

import nlp4s.english.Lexicon
import nlp4s.english.lexicon._

trait StandardVerbs { self: Lexicon.Builder =>
  addEntries(List(

    IntransitiveVerb("move", "moves", "move", "moving", "moved", "moved"),
    IntransitiveVerb("walk", "walks", "walk", "walking", "walked", "walked"),
    IntransitiveVerb("crawl", "crawls", "crawl", "crawling", "crawled", "crawled"),
    IntransitiveVerb("run", "runs", "run", "running", "ran", "run"),
    IntransitiveVerb("drink", "drinks", "drink", "drinking", "drank", "drunken"),
    IntransitiveVerb("go", "goes", "go", "going", "went", "gone"),
    IntransitiveVerb("look", "looks", "look", "looking", "looked", "looked"),
    IntransitiveVerb("stand", "stands", "stand", "standing", "stood", "stood"),

    TransitiveVerb("move", "moves", "move", "moving", "moved", "moved"),
    TransitiveVerb("take", "takes", "take", "taking", "took", "taken"),
    TransitiveVerb("pick_up", "picks_up", "pick_up", "picking_up", "picked_up", "picked_up"),
    TransitiveVerb("pick", "picks", "pick", "picking", "picked", "picked"),
    TransitiveVerb("drink", "drinks", "drink", "drinking", "drank", "drunken"),
    TransitiveVerb("see", "sees", "see", "seeing", "saw", "seen"),
    TransitiveVerb("break", "breaks", "break", "breaking", "broke", "broken"),
    TransitiveVerb("put", "puts", "put", "putting", "put", "put"),
    TransitiveVerb("do", "does", "do", "doing", "did", "done"),
    TransitiveVerb("have", "has", "have", "having", "had", "had"),

    LinkVerb("look", "looks", "look", "looking", "looked", "looked"),
    LinkVerb("smell", "smells", "smell", "smelling", "smelled", "smelled"),
    LinkVerb("look", "looks", "look", "looking", "looked", "looked"),
    LinkVerb("sound", "sounds", "sound", "sounding", "sounded", "sounded"),

    HelpVerb("do", "does", "do", "doing", "did", "done"),

    ModalVerb("can"),
    ModalVerb("could"),
    ModalVerb("may"),
    ModalVerb("might"),
    ModalVerb("must"),
    ModalVerb("shall"),
    ModalVerb("should"),
    ModalVerb("will"),
    ModalVerb("would"),
    ModalVerb("ought_to"),
    ModalVerb("need_to"),
    // ModalVerb("have_to"),

    ModalVerb("can't", negate = true, Some("can")),
    ModalVerb("couldn't", negate = true, Some("could")),
    ModalVerb("mustn't", negate = true, Some("must")),
    ModalVerb("won't", negate = true, Some("will")),
    // ModalVerb("haven't", negate = true),

    ToBe(),
    ToHave(),
  ))
}
