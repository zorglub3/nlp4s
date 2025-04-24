package nlp4s.base

abstract class NlpError(message: String) {
  def getMessage(): String = message
}

case class GenericError(message: String) extends NlpError(message)
