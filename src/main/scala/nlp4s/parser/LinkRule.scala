package nlp4s.parser

import nlp4s.base.LinkTag

sealed abstract class Link(val linkTag: LinkTag, val isLeft: Boolean)

final case class LeftLink(lt: LinkTag) extends Link(lt, true)
final case class RightLink(lt: LinkTag) extends Link(lt, false)

final case class LinkRule(l: List[LeftLink], r: List[RightLink]) {
  def ++(other: LinkRule): LinkRule = LinkRule(l ++ other.l, r ++ other.r)
  def leftLinks = l.reverse
  def rightLinks = r.reverse

  def pp: String =
    s"${leftLinks.map(_.linkTag.name).mkString(" & ")} <-> ${rightLinks.map(_.linkTag.name).mkString(" & ")}"
}

object LinkRule {
  final case class NormalForm(disjunction: List[LinkRule]) {
    def |(other: NormalForm) = NormalForm(disjunction ++ other.disjunction)
    def &(other: NormalForm) =
      NormalForm(
        for {
          c1 <- disjunction
          c2 <- other.disjunction
        } yield c1 ++ c2 )
    def linkRules: List[LinkRule] = disjunction
  }
}

object LinkRuleSyntax {
  import LinkRule._

  def l(linkTag: LinkTag): NormalForm =
    NormalForm(List(LinkRule(List(LeftLink(linkTag)), List.empty)))

  def r(linkTag: LinkTag): NormalForm =
    NormalForm(List(LinkRule(List.empty, List(RightLink(linkTag)))))

  def empty: NormalForm =
    NormalForm(List(LinkRule(List.empty, List.empty)))

  def opt(r: NormalForm): NormalForm = empty | r
}
