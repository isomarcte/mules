package io.chrisdavenport.mules.layered

import cats._
import cats.data._
import cats.effect.IO
import cats.effect._
import cats.effect.testing.specs2.CatsIO
import cats.implicits._
import io.chrisdavenport.mules._
import org.specs2.mutable.Specification

final class LayeredLookupSpec extends Specification with CatsIO {
  import LayeredLookupSpec._

  val testKey0: String = "0"
  val testKey1: String = "1"
  val testKey2: String = "2"
  val testKey3: String = "3"
  val testValue0: Int = 0
  val testValue1: Int = 1

  "LayeredLookup.fromLookups" should {
    "not find values in an always empty lookup" in {
      val emptyLayeredCache: LayeredLookup[Id, String, Int] =
        LayeredLookup.fromLookups[Id, List, String, Int](List.empty)
      emptyLayeredCache.lookup(this.testKey0) must_=== None
    }

    "find the correct values in a single layer lookup" in {
      val setup: IO[Option[LayeredValue[Int]]] =
        for {
          cache0 <- cacheLayer
          _ <- cache0.insert(this.testKey0, this.testValue0)
          ll = LayeredLookup.fromLookups[IO, Option, String, Int](Some(cache0))
          result <- ll.layeredLookup(this.testKey0)
        } yield result
      setup.map(_ must_=== Some(LayeredValue(0, this.testValue0)))
    }

    "find the correct values in the correct layers" in {
      for {
        cache0 <- cacheLayer
        cache1 <- cacheLayer
        // Should find the value in layer 0 with value testValue0
        _ <- cache0.insert(this.testKey0, this.testValue0)
        _ <- cache1.insert(this.testKey0, this.testValue1)

        // Should find the value in layer 1 with value testValue0
        _ <- cache1.insert(this.testKey1, this.testValue0)

        // Should find the value in layer 0 with value testValue0
        _ <- cache0.insert(this.testKey2, this.testValue0)

        ll = LayeredLookup.fromLookups[IO, NonEmptyList, String, Int](
          NonEmptyList.of(cache0, cache1)
        )

        result0 <- ll.layeredLookup(this.testKey0)
        result1 <- ll.layeredLookup(this.testKey1)
        result2 <- ll.layeredLookup(this.testKey2)

        // Not in the cache
        result3 <- ll.layeredLookup(this.testKey3)
      } yield {
        result0 must_=== Some(LayeredValue(0, this.testValue0))
        result1 must_=== Some(LayeredValue(1, this.testValue0))
        result2 must_=== Some(LayeredValue(0, this.testValue0))
        result3 must_=== None
      }
    }
  }
}

object LayeredLookupSpec {
  def cacheLayer(implicit T: Timer[IO]): IO[Cache[IO, String, Int]] =
    MemoryCache.ofSingleImmutableMap[IO, String, Int](None)(Sync[IO], T.clock)
}
