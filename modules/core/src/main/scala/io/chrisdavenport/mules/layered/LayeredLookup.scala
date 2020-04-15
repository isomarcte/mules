package io.chrisdavenport.mules.layered

import cats._
import cats.implicits._
import io.chrisdavenport.mules._

trait LayeredLookup[F[_], K, V] extends Lookup[F, K, V] {
  protected def FFunctor: Functor[F]

  def layeredLookup_[G[_]: Alternative](k: K): F[G[LayeredValue[V]]]

  def layeredLookup(k: K): F[Option[LayeredValue[V]]] =
    this.layeredLookup_[Option](k)

  final def lookupInAllLayers(k: K): F[List[LayeredValue[V]]] =
    this.layeredLookup_[List](k)

  override final def lookup(k: K): F[Option[V]] =
    FFunctor.map(this.layeredLookup(k))(_.map(_.value))

}

object LayeredLookup {
  def fromLookups[F[_], G[_], K, V](lookups: G[Lookup[F, K, V]])(implicit F: Monad[F], G: Foldable[G]): LayeredLookup[F, K, V] =
    new LayeredLookup[F, K, V] {
      override final protected val FFunctor: Functor[F] = F

      override final def layeredLookup(k: K): F[Option[LayeredValue[V]]] =
        lookups.foldLeftM[F, (Int, Option[LayeredValue[V]])](Tuple2(0, Option.empty[LayeredValue[V]])){
          case ((idx, result @ Some(_)), _) =>
            // Short circuit. It is reasonable to assume that in most
            // implementations `F` will be a side-effecting type,
            // e.g. `IO`. In that case, when the caller only wants the _first_
            // cache value we don't actually want to keep calling the later
            // caches after we've already got a result.
            F.pure(Tuple2(idx + 1, result))
          case ((idx, _), lookup) =>
            lookup.lookup(k).map((result: Option[V]) =>
              (idx + 1, result.map(LayeredValue(idx, _))))
        }.map(_._2)

      override final def layeredLookup_[H[_]: Alternative](k: K): F[H[LayeredValue[V]]] = {
        val empty: H[LayeredValue[V]] = Alternative[H].empty
        lookups.foldLeftM[F, (Int, H[LayeredValue[V]])](Tuple2(0, empty)){
          case ((idx, acc), lookup) =>
            lookup.lookup(k).map(_.fold(
              empty
            )((result: V) =>
              Applicative[H].pure(LayeredValue(idx, result))
            )).map((result: H[LayeredValue[V]]) =>
              (idx + 1, acc <+> result)
            )
        }.map(_._2)
      }
    }
}
