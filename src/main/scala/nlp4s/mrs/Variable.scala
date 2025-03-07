package nlp4s.mrs

case class Variable(name: String)

object Variable {
  final case class Generator(counter: Int) {
    def generate(): (Generator, Variable) =
      (Generator(counter + 1), Variable(s"x$counter"))
  }

  def initGenerator: Generator =
    Generator(0)
}
