package io.chrisdavenport.mules.layered

import cats.effect.IO
import cats.effect._
import cats.effect.testing.specs2.CatsIO
import cats.implicits._
import org.specs2.mutable.Specification

final class LayeredDeleteSpec extends Specification with CatsIO {
  import TestValues._

  "LayeredDelete instances" should {
    "delete values in all layers with the default LayeredDelete.delete implementation" in {
      for {
        cache0 <- cacheLayer
        cache1 <- cacheLayer
        _ <- cache0.insert(testKey0, testValue0)
        _ <- cache1.insert(testKey0, testValue0)
        beforeDelete0 <- cache0.lookup(testKey0)
        beforeDelete1 <- cache1.lookup(testKey0)
        ld = LayeredDelete.fromDeletes[IO, List, String](List(cache0, cache1))
        _ <- ld.delete(testKey0)
        afterDelete0 <- cache0.lookup(testKey0)
        afterDelete1 <- cache1.lookup(testKey0)
      } yield {
        beforeDelete0 must_=== Some(testValue0)
        beforeDelete0 must_=== beforeDelete1
        afterDelete0 must_=== None
        afterDelete1 must_=== None
      }
    }

    "only delete values in the layers specified" in {
      for {
        cache0 <- cacheLayer
        cache1 <- cacheLayer
        cache2 <- cacheLayer
        _ <- cache0.insert(testKey0, testValue0)
        _ <- cache1.insert(testKey0, testValue0)
        _ <- cache2.insert(testKey0, testValue0)
        beforeDelete0 <- cache0.lookup(testKey0)
        beforeDelete1 <- cache1.lookup(testKey0)
        beforeDelete2 <- cache2.lookup(testKey0)
        ld = LayeredDelete.fromDeletes[IO, List, String](List(cache0, cache1, cache2))
        _ <- ld.layeredDelete(_ === 1)(testKey0)
        afterDelete0 <- cache0.lookup(testKey0)
        afterDelete1 <- cache1.lookup(testKey0)
        afterDelete2 <- cache2.lookup(testKey0)
      } yield {
        beforeDelete0 must_=== Some(testValue0)
        beforeDelete1 must_=== beforeDelete0
        beforeDelete2 must_=== beforeDelete0
        afterDelete0 must_=== beforeDelete0
        afterDelete1 must_=== None
        afterDelete2 must_=== beforeDelete0
      }
    }
  }
}
