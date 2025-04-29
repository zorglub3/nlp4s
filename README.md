# Natural language processing for Scala

This is a collection of various projects I've done over the years. They all relate to
processing natural language somehow. Currently, it consists of the following parts:

1. A tokenizer that can combine tokens such as "put" and "down" into "put\_down". This
   is useful when representing special word combinations (such as "put\_down") as single
   tokens in, eg., a parser.
2. A link parser that supports multiple parses of the same sentence. It can deal with 
   the preposition problem and more. As an example, it can recognise that "I saw the man
   with the telescope." can be parsed in two ways.
3. An MRS data structure with associated methods and functions.
4. A interpreter, that can transform the output of a link-parser, ie, a graph, to an MRS
   representation.
5. A text planner that can give an overall plan for a text (a list of clauses) when given
   a knowledge base, an initial focus, and a schema.
6. A surface realiser for mapping clauses to sentences (ie words and punctuation).
7. A printer for such sentences and punctuation (automatically choose to use capital
   letters and whitespaces correctly).
8. Specializations of the above 1-7 that deal with the English written language.

All parts are still work in progress. A demo can be used from the sbt console.

## Current issues and WIP

- realise imperative sentences. Use '!' for punctuation even? (English realiser)
- realise pronouns in sentences. (English realiser)
- Double parse of sentence "Are they are a group" (using eg `parseString` in the English demo
  yields two identical parse graphs - should be only one).
- Negation. Completely missing from `EnglishGraphInterpreter` and realiser.
- Parse future progressive, eg, "I will be taking the fork".
- Port more tests from the old nlp library.
- Quantifier scope resolution:
  1. It should work for _all_ valid MRS representations (never give empty answer)
  2. Write test suite
  3. Make it possible to give preferred/default ordering on quantifiers

# References and related work

- [Head-Driven Phrase Structure Grammar: The handbook, Second revised edition.](https://langsci-press.org/catalog/book/478) The guide to HPSG, which "inspires" some parts of this project.
- [Understading the Output of ERG](https://blog.inductorsoftware.com/blog/DelphinMRSOutput) A blog about using
  MRS as output from a parser. This project also parses to MRS, but in a different way using a different
  parser implemented in a different programming language.
- "Parsing English with a Link Grammar", Daniel D. K. Sleator and Davy
  Temperly, 1991
- [The CMU link grammar natural language parser](https://github.com/opencog/link-grammar) is
  a C implementation based on the original CMU link parser. It has bindings
  to other languages including Java and thus Scala.
- [Minimal Recursion Semantics, An Introduction](https://www.cl.cam.ac.uk/~aac10/papers/mrs.pdf) 
  gives a basic overview of MRS and how it can be used for HPSG.

