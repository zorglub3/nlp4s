package nlp4s.base

abstract class NlpError(message: String) {
  def getMessage(): String = message
}
