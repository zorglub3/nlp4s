package nlp4s.textgen

import nlp4s.base.NlpError

case class TextGenError(msg: String) extends NlpError(msg)
