package io.chrisdavenport.mules.layered

import cats._
import cats.implicits._
import io.chrisdavenport.mules._

trait LayeredDelete[F[_], K] extends Delete[F, K] {

  protected def FApplicative: Applicative[F]

  def layeredDelete_(shouldDeleteFromLayerN: Int => F[Boolean])(k: K): F[Unit]

  final def layeredDelete(shouldDeleteFromLayerN: Int => Boolean)(k: K): F[Unit] =
    this.layeredDelete_((layer: Int) => this.FApplicative.pure(shouldDeleteFromLayerN(layer)))(k)

  override final def delete(k: K): F[Unit] =
    this.layeredDelete(Function.const(true))(k)
}

object LayeredDelete {
  def fromDeletes[F[_], G[_], K](deletes: G[Delete[F, K]])(implicit F: Monad[F], G: Foldable[G]): LayeredDelete[F, K] =
    new LayeredDelete[F, K] {
      override def FApplicative: Applicative[F] = F

      override final def layeredDelete_(shouldDeleteFromLayerN: Int => F[Boolean])(k: K): F[Unit] =
        deletes.foldLeftM[F, Int](0){
          case (layer, delete) =>
            shouldDeleteFromLayerN(layer).ifM(
              delete.delete(k),
              F.unit
            ) *> F.pure(layer + 1)
        }.void
    }
}
