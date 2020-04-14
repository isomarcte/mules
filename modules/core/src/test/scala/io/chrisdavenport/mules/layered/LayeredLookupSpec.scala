package io.chrisdavenport.mules.layered

import cats._
import cats.data._
import cats.effect.IO
import cats.effect._
import cats.effect.testing.specs2.CatsIO
import cats.implicits._
import org.specs2.mutable.Specification

final class LayeredLookupSpec extends Specification with CatsIO {
  import TestValues._

  "LayeredLookup instances" should {
    "not find values in an always empty lookup" in {
      val emptyLayeredCache: LayeredLookup[Id, String, Int] =
        LayeredLookup.fromLookups[Id, List, String, Int](List.empty)
      emptyLayeredCache.lookup(testKey0) must_=== None
    }

    "find the correct values in a single layer lookup" in {
      val setup: IO[Option[LayeredValue[Int]]] =
        for {
          cache0 <- cacheLayer
          _ <- cache0.insert(testKey0, testValue0)
          ll = LayeredLookup.fromLookups[IO, Option, String, Int](Some(cache0))
          result <- ll.layeredLookup(testKey0)
        } yield result
      setup.map(_ must_=== Some(LayeredValue(0, testValue0)))
    }

    "find the correct values in the correct layers" in {
      for {
        cache0 <- cacheLayer
        cache1 <- cacheLayer
        // Should find the value in layer 0 with value testValue0
        _ <- cache0.insert(testKey0, testValue0)
        _ <- cache1.insert(testKey0, testValue1)

        // Should find the value in layer 1 with value testValue0
        _ <- cache1.insert(testKey1, testValue0)

        // Should find the value in layer 0 with value testValue0
        _ <- cache0.insert(testKey2, testValue0)

        ll = LayeredLookup.fromLookups[IO, NonEmptyList, String, Int](
          NonEmptyList.of(cache0, cache1)
        )

        result0 <- ll.layeredLookup(testKey0)
        result1 <- ll.layeredLookup(testKey1)
        result2 <- ll.layeredLookup(testKey2)

        // Not in the cache
        result3 <- ll.layeredLookup(testKey3)
      } yield {
        result0 must_=== Some(LayeredValue(0, testValue0))
        result1 must_=== Some(LayeredValue(1, testValue0))
        result2 must_=== Some(LayeredValue(0, testValue0))
        result3 must_=== None
      }
    }
  }
}
