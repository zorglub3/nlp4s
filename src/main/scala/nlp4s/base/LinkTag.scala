package nlp4s.base

class LinkTag(val name: String) {
  def matches(other: String): Boolean = 
    name.startsWith(other) || other.startsWith(name)
  def matches(other: LinkTag): Boolean =
    matches(other.name)

  def simplify: LinkTag = new LinkTag(name.take(1))

  def ==(other: LinkTag): Boolean = name == other.name

  override def equals(other: Any): Boolean = {
    other match {
      case x: LinkTag => x.name == name
      case _ => false
    }
  }

  override def hashCode(): Int = name.hashCode

  override def toString(): String = s"LinkTag($name)"
}
