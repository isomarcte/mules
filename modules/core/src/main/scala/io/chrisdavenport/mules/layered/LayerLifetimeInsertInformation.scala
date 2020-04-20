package io.chrisdavenport.mules.layered

import io.chrisdavenport.mules._

final case class LayerLifetimeInsertInformation[F[_]](
  shouldInsertToLayer: Int => F[Boolean],
  layerTimespec: Int => F[Option[TimeSpec]]
)
