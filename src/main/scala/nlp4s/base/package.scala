package nlp4s

package object base {
  type NlpResult[A] = Either[NlpError, A]
}
