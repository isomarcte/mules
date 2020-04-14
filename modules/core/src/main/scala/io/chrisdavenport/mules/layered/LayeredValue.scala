package io.chrisdavenport.mules.layered

final case class LayeredValue[V](
  cacheLayer: Int,
  value: V
)
