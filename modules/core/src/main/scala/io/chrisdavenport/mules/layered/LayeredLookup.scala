package io.chrisdavenport.mules.layered

import cats._
import cats.implicits._
import io.chrisdavenport.mules._

trait LayeredLookup[F[_], K, V] extends Lookup[F, K, V] {
  protected def FFunctor: Functor[F]

  override final def lookup(k: K): F[Option[V]] =
    FFunctor.map(this.layeredLookup(k))(_.map(_.value))
  def layeredLookup(k: K): F[Option[LayeredValue[V]]]
}

object LayeredLookup {
  def fromLookups[F[_], G[_], K, V](lookups: G[Lookup[F, K, V]])(implicit F: Monad[F], G: Foldable[G]): LayeredLookup[F, K, V] =
    new LayeredLookup[F, K, V] {
      override final protected val FFunctor: Functor[F] = F

      override final def layeredLookup(k: K): F[Option[LayeredValue[V]]] =
        lookups.foldLeftM[F, (Int, Option[LayeredValue[V]])](Tuple2(0, Option.empty[LayeredValue[V]])){
          case (result @ (_, Some(_)), _) =>
            F.pure(result)
          case ((idx, _), value) =>
            value.lookup(k).map(_.fold(
              (idx + 1, Option.empty[LayeredValue[V]])
            )((result: V) =>
              // -1 because the index should never be used after this
              // point. Thus, -1 simply becomes an oracle for debugging.
              (-1, Some(LayeredValue(idx, result)))
            ))
        }.map(_._2)
    }
}
