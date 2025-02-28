package nlp4s.parser

import nlp4s.base.NlpError

case class NoParse() extends NlpError("No parse for sentence")
