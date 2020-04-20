package io.chrisdavenport.mules.layered

import cats._
import cats.implicits._
import io.chrisdavenport.mules._

trait LayeredInsert[F[_], K, V] extends Insert[F, K, V] {
  protected def FApplicative: Applicative[F]

  def layeredInsert_(shouldInsertToLayerN: Int => F[Boolean])(k: K, v: V): F[Unit]

  final def layeredInsert(shouldInsertToLayerN: Int => Boolean)(k: K, v: V): F[Unit] =
    this.layeredInsert_((layer: Int) => this.FApplicative.pure(shouldInsertToLayerN(layer)))(k, v)
}

object LayeredInsert {
  def fromInserts[F[_], G[_], K, V](inserts: G[Insert[F, K, V]])(implicit F: Monad[F], G: Foldable[G]): LayeredInsert[F, K, V] =
    new LayeredInsert[F, K, V] {
      override final protected val FApplicative: Applicative[F] =
        F

      override final def layeredInsert_(shouldInsertToLayerN: Int => F[Boolean])(k: K, v: V): F[Unit] =
        inserts.foldLeftM[F, Int](0){
          case (layer, insert) =>
            shouldInsertToLayerN(layer).ifM(
              insert.insert(k, v),
              F.unit
            ) *> F.pure(layer + 1)
        }.void

      override final def insert(k: K, v: V): F[Unit] =
        this.layeredInsert(Function.const(true))(k, v)
    }
}
