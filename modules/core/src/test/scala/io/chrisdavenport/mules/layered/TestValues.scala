package io.chrisdavenport.mules.layered

import cats.effect._
import io.chrisdavenport.mules._

object TestValues {
  val testKey0: String = "0"
  val testKey1: String = "1"
  val testKey2: String = "2"
  val testKey3: String = "3"
  val testValue0: Int = 0
  val testValue1: Int = 1

  def cacheLayer(implicit T: Timer[IO]): IO[LifetimeCache[IO, String, Int]] =
    MemoryCache.ofSingleImmutableMap[IO, String, Int](None)(Sync[IO], T.clock)
}
