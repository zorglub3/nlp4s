package nlp4s.mrs

case class Handle(n: Int) {
  def asString = s"h$n"
}

object Handle {
  final case class Generator(counter: Int) {
    def generate(): (Generator, Handle) =
      (Generator(counter + 1), Handle(counter))
  }

  def initGenerator: Generator = 
    Generator(0)
}
