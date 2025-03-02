package nlp4s.mrs

import nlp4s.base.NlpError

case class InterpreterError(message: String) extends NlpError(message)
