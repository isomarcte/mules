package io.chrisdavenport.mules.layered

import cats.effect._
import cats.effect.testing.specs2.CatsIO
import cats.implicits._
import io.chrisdavenport.mules._
import java.util.concurrent.TimeUnit
import org.specs2.mutable._
import scala.concurrent.duration._

final class LayeredLifetimeInsertSpec extends Specification with CatsIO {
  import LayeredLifetimeInsertSpec._
  import TestValues._

  "LayeredLifetimeInsert instances" should {
    "should insert into all layers with the defaultExpiration when calling .insert" in {
      val timer: Timer[IO] = Timer[IO]
      for {
        cache0 <- cacheLayer
        cache1 <- cacheLayer
        start <- timer.clock.monotonic(TimeUnit.MILLISECONDS)
        lli = LayeredLifetimeInsert.fromLifetimeInserts[IO, List, String, Int](List(cache0, cache1), Function.const(IO.pure(None)), Some(defaultDefaultExpirationForInsertAllTimeSpec))
        _ <- lli.insert(testKey0, testValue0)
        t0Lookup0 <- cache0.lookup(testKey0)
        t0Lookup1 <- cache1.lookup(testKey0)
        t0End <- timer.clock.monotonic(TimeUnit.MILLISECONDS)
        // Sleep until we are _sure_ the expiration has occurred.
        t0Time = FiniteDuration(t0End - start, TimeUnit.MILLISECONDS)
        scaledSleepDuration = defaultDefaultExpirationForInsertAll * 2L
        _ <- timer.sleep(scaledSleepDuration - t0Time)
        t1Lookup0 <- cache0.lookup(testKey0)
        t1Lookup1 <- cache1.lookup(testKey0)
      } yield {
        // These should _always_ be empty.
        t1Lookup0 must_=== None
        t1Lookup1 must_=== t1Lookup0

        // T0 Checks
        t0Time must beLessThan(defaultDefaultExpirationForInsertAll)
        t0Lookup0 must_=== Some(testValue0)
        t0Lookup1 must_=== t0Lookup0
      }
    }

    "should insert into the correct layers with the correct timeouts" in {
      val timer: Timer[IO] = Timer[IO]
      val duration0: FiniteDuration =
        FiniteDuration(1L, TimeUnit.SECONDS)
      val otherDuration: FiniteDuration =
        FiniteDuration(2L, TimeUnit.SECONDS)
      val llii: LayerLifetimeInsertInformation[IO] =
        LayerLifetimeInsertInformation(
          (layer: Int) => IO.pure(layer =!= 1),
          (layer: Int) => layer match {
            case 0 => IO.pure(TimeSpec.fromDuration(duration0))
            case _ => IO.pure(TimeSpec.fromDuration(otherDuration))
          }
        )
      for {
        cache0 <- cacheLayer
        cache1 <- cacheLayer
        cache2 <- cacheLayer
        start <- timer.clock.monotonic(TimeUnit.MILLISECONDS)
        lli = LayeredLifetimeInsert.fromLifetimeInserts[IO, List, String, Int](List(cache0, cache1, cache2), Function.const(IO.pure(None)), None)
        _ <- lli.layeredInsertWithTimeout(llii)(testKey0, testValue0)
        t0Lookup0 <- cache0.lookup(testKey0)
        t0Lookup1 <- cache1.lookup(testKey0)
        t0Lookup2 <- cache2.lookup(testKey0)
        t0End <- timer.clock.monotonic(TimeUnit.MILLISECONDS)
        // Sleep until we are _sure_ the cache0 expiration has occurred.
        t0Time = FiniteDuration(t0End - start, TimeUnit.MILLISECONDS)
        scaledSleepDuration0 = duration0 + (FiniteDuration(250L, TimeUnit.MILLISECONDS))
        _ <- timer.sleep(scaledSleepDuration0 - t0Time)
        t1Lookup0 <- cache0.lookup(testKey0)
        t1Lookup1 <- cache1.lookup(testKey0)
        t1Lookup2 <- cache2.lookup(testKey0)
        t1End <- timer.clock.monotonic(TimeUnit.MILLISECONDS)
        // Sleep until we are _sure_ the other caches have expiration has occurred.
        t1Time = FiniteDuration(t1End - start, TimeUnit.MILLISECONDS)
        scaledSleepDuration1 = otherDuration + (FiniteDuration(50L, TimeUnit.MILLISECONDS))
        _ <- timer.sleep(scaledSleepDuration1 - t1Time)
        t2End <- timer.clock.monotonic(TimeUnit.MILLISECONDS)
        t2Time = FiniteDuration(t2End - start, TimeUnit.MILLISECONDS)
        t2Lookup0 <- cache0.lookup(testKey0)
        t2Lookup1 <- cache1.lookup(testKey0)
        t2Lookup2 <- cache2.lookup(testKey0)
      } yield {
        // T0
        t0Lookup0 must_=== Some(testValue0)
        t0Lookup1 must_=== None
        t0Lookup2 must_=== t0Lookup0

        // T1
        t1Time must beGreaterThan(duration0)
        t1Time must beLessThan(otherDuration)
        t1Lookup0 must_=== None
        t1Lookup1 must_=== t1Lookup0
        t1Lookup2 must_=== Some(testValue0)

        // T2
        t2Time must beGreaterThan(otherDuration)
        t2Lookup0 must_=== None
        t2Lookup1 must_=== t2Lookup0
        t2Lookup2 must_=== t2Lookup1
      }
    }
  }
}

object LayeredLifetimeInsertSpec {
  val defaultDefaultExpirationForInsertAll: FiniteDuration =
    FiniteDuration(1L, TimeUnit.SECONDS)
  lazy val defaultDefaultExpirationForInsertAllTimeSpec: TimeSpec =
    TimeSpec.unsafeFromDuration(this.defaultDefaultExpirationForInsertAll)
}
