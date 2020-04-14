package io.chrisdavenport.mules.layered

import cats.effect.IO
import cats.effect._
import cats.effect.testing.specs2.CatsIO
import cats.implicits._
import org.specs2.mutable.Specification

final class LayeredInsertSpec extends Specification with CatsIO {
  import TestValues._

  "LayeredInsert instances" should {
    "only insert values into the specified layers" in {

      // Only insert into layer 0 and 3, i.e. the first and third caches.
      val shouldInsertToLayerN: Int => Boolean =
        (layer: Int) => layer === 0 || layer === 2

      for {
        cache0 <- cacheLayer
        cache1 <- cacheLayer
        cache2 <- cacheLayer
        layeredInsert = LayeredInsert.fromInserts[IO, List, String, Int](List(cache0, cache1, cache2))
        _ <- layeredInsert.layeredInsert(shouldInsertToLayerN)(testKey0, testValue0)
        layer0Value <- cache0.lookup(testKey0)
        layer1Value <- cache1.lookup(testKey0)
        layer2Value <- cache2.lookup(testKey0)
      } yield {
        layer0Value must_=== Some(testValue0)
        layer1Value must_=== None
        layer2Value must_=== Some(testValue0)
      }
    }
    "insert values into _all_ layers via the insert method" in {
      for {
        cache0 <- cacheLayer
        cache1 <- cacheLayer
        cache2 <- cacheLayer
        layeredInsert = LayeredInsert.fromInserts[IO, List, String, Int](List(cache0, cache1, cache2))
        _ <- layeredInsert.insert(testKey0, testValue0)
        layer0Value <- cache0.lookup(testKey0)
        layer1Value <- cache1.lookup(testKey0)
        layer2Value <- cache2.lookup(testKey0)
      } yield {
        layer0Value must_=== Some(testValue0)
        layer1Value must_=== Some(testValue0)
        layer2Value must_=== Some(testValue0)
      }
    }
  }
}
