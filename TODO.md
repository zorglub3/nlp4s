# TODO

## Refactor and improvements

### Tokenizer
- [X] support ' (ping) as in can't or won't
- the ping should still be there for text generation

### Support _all_ modal auxiliary verbs
- [X] can could may might must shall should will would ought(to) need(to) have(to)
- [X] support negative forms as well: can't couldn't mustn't won't haven't 
- [X] note: "will" is aux verb, treat it as such (not special case for future tense)
- see [wikipedia](https://en.wikipedia.org/wiki/English_modal_auxiliary_verbs).
- [X] also negatives
- double negatives as in "won't he not take the bus" (shouldn't be valid sentence - how to interpret???)

### Parser
- Use link tags that match CMU link parser
- [X] Parse future progressive, eg, "I will be taking the fork"
  + [X] treat "will" as aux/modal verb
  + same as "I may be taking the fork" or "he must be taking the fork"
- complex sentences: connectives and sub-ordinate sentences

### Interpreter
- fix predicate for possessives, eg, "the mans leg"
- [X] 'implicit' unary relation is a globalrelation - not a quantified one
- [X] pronoun relation is a global relation - not quantified
- [X] Questions with 'who' and 'what'
- [X] Negation
- Adverbs and prepositions
  + Adverbs are either scopal relations or global (not local)
  + Prepositions cannot be global (quantified over noun phrase argument)
- [X] Aux function - different kinds of questions:
  + "who took the thing" - no aux verb, head: "who"
  + "why did he take the thing" - aux verb, verb order, head: "why"
  + "did he take the thing" - aux verb, verb order
  + "can he take the thing" - modal verb, verb order
  + "how can he take the thing" - modal verb, verb order, head: "how"
  + "how did he take the thing" - aux verb, verb order, head: "how"
  + "who will take the thing" - no verb order mix, head: "who"
  + "how will he take the thing" - aux verb, verb order, head: "how"
- Nominal sentences
  + No verb, but must be something for tense and mode relations
  + "he was old" is different from "he will be old"
  + Also, works with questions and modal- verbs and question- modes
- [X] Present and past participle
  + [X] Also with questions
  + [X] with modal verbs, eg, "he will be running" and "will he be running"
  + what about "he will have taken" (I think it should be good)

### MRS
- builder: fix local top (point to top-most non-quantifier)
- gender on third person singular pronouns

### Reference resolution
- Generic context structure: speaker, listener, in-scope, pronouns, etc.
- A resolution result is always "inside" a context
- Test for reference resolution

### Quantifier scope resolution
- never give empty answer for valid MRS
- test suite
- allow default/preferred quantifier ordering

### Realiser
- Adverbs
- Prepositions
- Questions
- Aux- and modal- verbs 
  + add modal verbs to wordbook
- Negation: either with "not" or special aux verbs (eg "won't")
- nominal sentences

## Bugs

### Parser
- Double parse of sentence "Are they a group" yields two identical graphs
