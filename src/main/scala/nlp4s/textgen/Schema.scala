package nlp4s.textgen

import scala.language.implicitConversions

case class Schema(items: List[Schema.Item]) extends Schema.IteratorFactory {
  def next(): List[Schema.Iterator] = Schema.iterateNext(items)
}

object Schema {
  trait Item {
    def |(other: Item): Item = {
      (this, other) match {
        case (Disjunction(i1), Disjunction(i2)) => Disjunction(i1 ++ i2)
        case (_, Disjunction(i2)) => Disjunction(this +: i2)
        case (Disjunction(i1), _) => Disjunction(i1 :+ other)
        case _ => Disjunction(List(this, other))
      }
    }
  }

  case class Disjunction(items: List[Item]) extends Item {
    def tail = Disjunction(items.tail)
  }

  case class Optional(item: Item) extends Item

  case class Predicate(pred: RhetoricalPredicate) extends Item

  case class Repeat0(item: Item) extends Item
  case class Repeat1(item: Item) extends Item

  class Recurse(schema: => Schema) extends Item {
    def getSchema(): Schema = schema
  }

  sealed trait IteratorFactory {
    def next(): List[Iterator]
  }

  sealed trait Iterator extends IteratorFactory {
    def predicate: Option[RhetoricalPredicate]
    def isEmpty: Boolean = predicate.nonEmpty
    def next(): List[Iterator]
  }

  def iterateNext(r: Iterable[Item]): List[Iterator] = {
    r.headOption match {
      case None => List(Done)
      case Some(Disjunction(l)) if l.nonEmpty => l.map { i => iterateNext(List(i) ++ r.tail) } .flatten
      case Some(Disjunction(Nil)) => iterateNext(r.tail)
      case Some(Optional(item)) => iterateNext(List(item)) ++ iterateNext(r.tail)
      case Some(Predicate(p)) => List(NotDone(p, r.tail))
      case Some(Repeat0(item)) => iterateNext(List(item)) ++ iterateNext(r.tail)  
      case Some(Repeat1(item)) => iterateNext(List(item, Repeat0(item)) ++ r.tail)
      case Some(e) => {
        e match { 
          case e: Recurse => iterateNext(r.tail) // NOTE - schema recursion is switched off for now
          case _ => List(Done)
        }
      }
    }
  }

  case object Done extends Iterator {
    def predicate = None
    def next() = List.empty
  }

  case class NotDone(
    pred: RhetoricalPredicate,
    rest: Iterable[Item],
  ) extends Iterator {
    def predicate = Some(pred)
    def next() = iterateNext(rest)
  }
}

object SchemaSyntax {
  def schema(items: Schema.Item*): Schema = Schema(items.toList)

  def optional(item: Schema.Item): Schema.Item = Schema.Optional(item)

  def repeat0(item: Schema.Item): Schema.Item = Schema.Repeat0(item)
  def repeat1(item: Schema.Item): Schema.Item = Schema.Repeat1(item)

  implicit def toSchemaItem(pred: RhetoricalPredicate): Schema.Item = Schema.Predicate(pred)
}
