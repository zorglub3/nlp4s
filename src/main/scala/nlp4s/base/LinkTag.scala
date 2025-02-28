package nlp4s.base

case class LinkTag(name: String) {
  def matches(other: String): Boolean = 
    name.startsWith(other) || other.startsWith(name)
  def matches(other: LinkTag): Boolean =
    matches(other.name)

  def simplify: LinkTag = LinkTag(name.take(1))
}
