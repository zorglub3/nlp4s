package nlp4s.textgen

import cats.data.StateT
import cats.data.WriterT
import cats.syntax.all._
import nlp4s.base.NlpResult

trait TextGenLang[SentenceSpec, Entity, K] {
  // provided by implementor
  def clausePotentialFocus(s: SentenceSpec): List[Entity]
  def makeSentenceSpec(e: Entity, rp: RhetoricalPredicate): F[SentenceSpec]

  // provided by this trait
  type W[T] = WriterT[Option, List[SentenceSpec], T]
  type F[T] = StateT[W, TextGenState, T]

  case class TextGenState(
    focus: Focus[Entity],
  )

  def writeSentence(rp: RhetoricalPredicate): F[Unit] = {
    val write: F[Unit] = {
      for {
        focus <- currentFocus()
        s <- makeSentenceSpec(focus, rp)
        pfl = clausePotentialFocus(s)
        _ <- tell(s)
        _ <- setPFL(pfl)
      } yield ()
    }

    def useFocus(f: Entity): F[Unit] =
      pushFocus(f) >> write

    def oneOf(foci: List[Entity]): F[Unit] = {
      foci match {
        case Nil => fail
        case h::t => useFocus(h) <+> oneOf(t)
      }
    }

    (potentialFocus() >>= oneOf) <+> write <+> (popFocus() >> write)
  }

  def pure[T](v: T): F[T] = 
    StateT.liftF(WriterT.value(v))

  def tell(s: SentenceSpec): F[Unit] = 
    StateT.liftF(WriterT.tell(List(s)))

  def setPFL(fl: List[Entity]): F[Unit] =
    StateT.modify { s => s.copy(focus = s.focus.setPFL(fl)) }

  def pushFocus(e: Entity): F[Unit] = 
    StateT.modify { s => s.copy(focus = s.focus.push(e)) } 

  def popFocus(): F[Unit] = 
    StateT.modify { s => s.copy(focus = s.focus.pop()) } 

  def currentFocus(): F[Entity] = 
    StateT.get[W, TextGenState].map(_.focus.immediateFocus)

  def potentialFocus(): F[List[Entity]] = {
    for {
      pfl <- StateT.inspect[W, TextGenState, List[Entity]](_.focus.focusList)
      visited <- StateT.inspect[W, TextGenState, Set[Entity]](_.focus.visited)
    } yield pfl.filter(!visited.contains(_))
  }

  def init(originalFocus: Entity): TextGenState = 
    TextGenState(Focus.init(originalFocus))

  def fail: F[Unit] =
    StateT.liftF(WriterT.valueT(None))

  def run[T](prg: F[T], originalFocus: Entity): NlpResult[List[SentenceSpec]] =
    prg.runS(init(originalFocus)).run match {
      case Some((state, _)) => Right(state)
      case None => Left(TextGenError("Could not produce text"))
    }

  def useSchemaItem(item: Schema.Item): F[Unit] = {
    import Schema._

    item match {
      case Disjunction(items) => {
        items.headOption match {
          case None => fail
          case Some(h) => useSchemaItem(h) <+> useSchemaItem(Disjunction(items.tail))
        }
      }
      case Optional(item) => useSchemaItem(item) <+> pure(()) 
      case Predicate(pred) => writeSentence(pred)
      case Repeat0(i) => {
        (for {
          _ <- useSchemaItem(i)
          _ <- useSchemaItem(item) <+> pure(())
        } yield ()) <+> pure(())
      }
      case Repeat1(i) => {
        for {
          _ <- useSchemaItem(i)
          _ <- useSchemaItem(Repeat0(i)) <+> pure(())
        } yield ()
      }
      case recurse: Recurse => useSchema(recurse.getSchema()) // TODO generate new global focus
      case _ => fail
    }
  }

  def useSchema(schema: Schema): F[Unit] =
    schema.items.map(useSchemaItem).sequenceVoid
}

