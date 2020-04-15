package io.chrisdavenport.mules.layered

import cats._
import cats.implicits._
import io.chrisdavenport.mules._

object ReportingLookup {
  def fromLookup[F[_]: FlatMap, K, V](backingLookup: Lookup[F, K, V], report: K => Option[V] => F[Unit]): Lookup[F, K, V] =
    new Lookup[F, K, V] {
      override final def lookup(k: K): F[Option[V]] =
        backingLookup.lookup(k).flatTap((result: Option[V]) =>
          report(k)(result)
        )
    }
}
