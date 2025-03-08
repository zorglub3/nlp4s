package nlp4s.realiser

import nlp4s.base.NlpResult
import nlp4s.mrs.Relation
import nlp4s.mrs.Variable
import nlp4s.mrs.MRS
import cats.data.WriterT
import cats.data.StateT

abstract class MRSRealiser {
  type W[T] = WriterT[Option, List[String], T]
  type F[T] = StateT[W, RealiserState, T]

  case class RealiserState(
    variableRelations: Map[Variable, Relation[Relation.Recursive]],
  )

  def init(mrs: MRS): RealiserState = RealiserState(Map.empty)

  def fail: F[Unit] = StateT.liftF(WriterT.valueT(None))
  def pure[T](v: T): F[T] = StateT.liftF(WriterT.value(v))
  def tell(s: String): F[Unit] = StateT.liftF(WriterT.tell(List(s)))
  def tellMore(ss: List[String]): F[Unit] = StateT.liftF(WriterT.tell(ss))

  def run(mrs: Relation.Recursive): NlpResult[List[String]] = ???
}
