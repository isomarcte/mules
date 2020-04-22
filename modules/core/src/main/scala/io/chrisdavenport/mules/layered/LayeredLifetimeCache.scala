package io.chrisdavenport.mules.layered

import cats._
import cats.implicits._
import io.chrisdavenport.mules._

trait LayeredLifetimeCache[F[_], K, V] extends LifetimeCache[F, K, V] with LayeredLifetimeInsert[F, K, V] with LayeredDelete[F, K] with LayeredLookup[F, K, V]

object LayeredLifetimeCache {
  type Layer[F[_], K, V] = Tuple3[Lookup[F, K, V], LifetimeInsert[F, K, V], Delete[F, K]]

  def fromLayers[F[_], G[_], K, V](
    layers: G[Layer[F, K, V]],
    defaultExpirationForLayers: Int => F[Option[TimeSpec]],
    defaultExpirationForInsertAll: Option[TimeSpec]
  )(
    implicit F: Monad[F],
             G: Foldable[G]
  ): LayeredLifetimeCache[F, K, V] = {
    val (lookups, inserts, deletes) =
      layers.foldMap{
        case (a, b, c) =>
          Tuple3(Vector(a), Vector(b), Vector(c))
      }

    val ll: LayeredLookup[F, K, V] =
      LayeredLookup.fromLookups(lookups)

    val lli: LayeredLifetimeInsert[F, K, V] =
      LayeredLifetimeInsert.fromLifetimeInserts(
        inserts,
        defaultExpirationForLayers,
        defaultExpirationForInsertAll
      )

    val ld: LayeredDelete[F, K] =
      LayeredDelete.fromDeletes(deletes)

    new LayeredLifetimeCache[F, K, V] {
      override protected final val FApplicative: Applicative[F] = F
      override protected final val FFunctor: Functor[F] = F

      override final val defaultExpiration: Option[TimeSpec] =
        lli.defaultExpiration

      override final def layeredDelete_(shouldDeleteFromLayerN: Int => F[Boolean])(k: K): F[Unit] =
        ld.layeredDelete_(shouldDeleteFromLayerN)(k)

      override final def defaultExpirationForLayer(layer: Int): F[Option[TimeSpec]] =
        lli.defaultExpirationForLayer(layer)

      override final def layeredInsertWithTimeout(layerLifetimeInsertInformation: LayerLifetimeInsertInformation[F])(k: K, v: V): F[Unit] =
        lli.layeredInsertWithTimeout(layerLifetimeInsertInformation)(k, v)

      override final def layeredLookup_[H[_]: Alternative](k: K): F[H[LayeredValue[V]]] =
        ll.layeredLookup_(k)
    }
  }

  // def fromLifetimeCaches[F[_]: Monad, G[_]: Functor]
}
