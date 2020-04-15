package io.chrisdavenport.mules.layered

import cats._
import cats.data._
import cats.effect.IO
import cats.effect._
import cats.effect.concurrent._
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

      "return all cache values when using an appropriate Altnerative instance" in {
        for {
          cache0 <- cacheLayer
          cache1 <- cacheLayer
          cache2 <- cacheLayer
          // Should find the value in layer 0 with value testValue0
          _ <- cache0.insert(testKey0, testValue0)
          _ <- cache2.insert(testKey0, testValue1)

          ll = LayeredLookup.fromLookups[IO, NonEmptyList, String, Int](
            NonEmptyList.of(cache0, cache1, cache2)
          )

          results <- ll.layeredLookup_[Chain](testKey0)
        } yield {
          results must_=== Chain(LayeredValue(0, testValue0), LayeredValue(2, testValue1))
        }
      }

      "return the same values for the layeredLookup and layeredLookup_[Option], ignoring values in higher layers" in {
        for {
          cache0 <- cacheLayer
          cache1 <- cacheLayer
          cache2 <- cacheLayer
          // Should find the value in layer 0 with value testValue0
          _ <- cache0.insert(testKey0, testValue0)
          _ <- cache2.insert(testKey0, testValue1)

          ll = LayeredLookup.fromLookups[IO, NonEmptyList, String, Int](
            NonEmptyList.of(cache0, cache1, cache2)
          )

          result <- ll.layeredLookup(testKey0)
          result_ <- ll.layeredLookup_[Option](testKey0)
        } yield {
          result must_=== result_
          result must_=== Some(LayeredValue(0, testValue0))
          result_ must_=== Some(LayeredValue(0, testValue0))
        }
      }

      "short circuit for layeredLookup, but not layeredLookup_[Option]" in {
        val reports: IO[Ref[IO, Map[Int, Map[String, Int]]]] = Ref.of[IO, Map[Int, Map[String, Int]]](Map.empty)

        // Keeps track of how many times a key was requested per layer.
        def reporter(
          reports: Ref[IO, Map[Int, Map[String, Int]]]
        )(
          layer: Int
        ): String => Option[Int] => IO[Unit] =
          k => _ => reports.update{m =>
            val updatedLayerMap: Map[String, Int] = {
              val layerMap: Map[String, Int] =
                m.getOrElse(layer, Map.empty[String, Int])
              layerMap + (k -> (layerMap.getOrElse(k, 0) + 1))
            }
            m + (layer -> updatedLayerMap)
          }

        for {
          reportsRef <- reports
          reporterF = reporter(reportsRef) _
          cache0 <- cacheLayer
          cache1 <- cacheLayer
          cache2 <- cacheLayer
          // Should find the value in layer 0 with value testValue0
          _ <- cache0.insert(testKey0, testValue0)
          _ <- cache2.insert(testKey0, testValue1)

          ll = LayeredLookup.fromLookups[IO, NonEmptyList, String, Int](
            NonEmptyList.of(
              ReportingLookup.fromLookup(cache0, reporterF(0)),
              ReportingLookup.fromLookup(cache1, reporterF(1)),
              ReportingLookup.fromLookup(cache2, reporterF(2))
            )
          )

          result <- ll.layeredLookup(testKey0)
          result_ <- ll.layeredLookup_[Option](testKey0)
          reportMap <- reportsRef.get
        } yield {
          result must_=== result_
          result must_=== Some(LayeredValue(0, testValue0))
          result_ must_=== Some(LayeredValue(0, testValue0))

          // Key was in layer 0, so it should have been requested twice. Once
          // by layeredLookup and once by layeredLookup_[Option].
          reportMap.getOrElse(0, Map.empty).getOrElse(testKey0, 0) must_=== 2

          // Since the key was in layer 0, layers 1 and 2 should have only
          // been requested by layeredLookup_[Option] once per layer.
          reportMap.getOrElse(1, Map.empty).getOrElse(testKey0, 0) must_=== 1
          reportMap.getOrElse(1, Map.empty).getOrElse(testKey0, 0) must_=== 1
        }
      }

      "return all cache values with lookupInAllLayers" in {
        for {
          cache0 <- cacheLayer
          cache1 <- cacheLayer
          cache2 <- cacheLayer
          // Should find the value in layer 0 with value testValue0
          _ <- cache0.insert(testKey0, testValue0)
          _ <- cache2.insert(testKey0, testValue1)

          ll = LayeredLookup.fromLookups[IO, NonEmptyList, String, Int](
            NonEmptyList.of(cache0, cache1, cache2)
          )

          results <- ll.lookupInAllLayers(testKey0)
        } yield {
          results must_=== List(LayeredValue(0, testValue0), LayeredValue(2, testValue1))
        }
      }
    }
  }
}
