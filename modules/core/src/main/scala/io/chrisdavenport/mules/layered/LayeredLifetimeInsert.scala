package io.chrisdavenport.mules.layered

import cats._
import cats.implicits._
import io.chrisdavenport.mules._

trait LayeredLifetimeInsert[F[_], K, V] extends LifetimeInsert[F, K, V] with LayeredInsert[F, K, V] {

  def defaultExpirationForLayer(layer: Int): F[Option[TimeSpec]]

  def layeredInsertWithTimeout(layerLifetimeInsertInformation: LayerLifetimeInsertInformation[F])(k: K, v: V): F[Unit]

  override final def layeredInsert_(shouldInsertToLayerN: Int => F[Boolean])(k: K, v: V): F[Unit] =
    this.layeredInsertWithTimeout(
      LayerLifetimeInsertInformation(
        shouldInsertToLayerN,
        (layer: Int) => this.defaultExpirationForLayer(layer)
      )
    )(k, v)

  override final def insertWithTimeout(optionTimeout: Option[TimeSpec])(k: K, v: V): F[Unit] =
    this.layeredInsertWithTimeout(
      LayerLifetimeInsertInformation(
        Function.const(FApplicative.pure(true)),
        Function.const(FApplicative.pure(optionTimeout)
        )
      )
    )(k, v)
}

object LayeredLifetimeInsert {
  def fromLifetimeInserts[F[_], G[_], K, V](
    lifetimeInserts: G[LifetimeInsert[F, K, V]],
    defaultExpirationForLayers: Int => F[Option[TimeSpec]],
    defaultExpirationForInsertAll: Option[TimeSpec]
  )(
    implicit F: Monad[F],
             G: Foldable[G]
  ): LayeredLifetimeInsert[F, K, V] =
    new LayeredLifetimeInsert[F, K, V] {
      override protected final val FApplicative: Applicative[F] = F

      override final val defaultExpiration: Option[TimeSpec] = defaultExpirationForInsertAll

      override final def defaultExpirationForLayer(layer: Int): F[Option[TimeSpec]] =
        defaultExpirationForLayers(layer)

      override final def layeredInsertWithTimeout(layerLifetimeInsertInformation: LayerLifetimeInsertInformation[F])(k: K, v: V): F[Unit] =
        lifetimeInserts.foldLeftM[F, Int](0){
          case (layer, lifetimeInsert) =>
            layerLifetimeInsertInformation.shouldInsertToLayer(layer).ifM(
              layerLifetimeInsertInformation.layerTimespec(layer).flatMap((optTimeSpec: Option[TimeSpec]) =>
                lifetimeInsert.insertWithTimeout(
                  optTimeSpec
                )(k, v)),
              F.unit
            ) *> F.pure(layer + 1)
        }.void
    }
}
