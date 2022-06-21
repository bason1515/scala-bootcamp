package com.evolutiongaming.bootcamp.effects.v3

import cats.effect.{Concurrent, ExitCode, IO, IOApp, Sync, Timer}
import cats.syntax.all._

import java.util.concurrent.{Executors, TimeoutException}
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Future, blocking}
import scala.util.Random
import scala.util.control.NonFatal

/*
 * Why cancellable IO is needed? Why it's better then Future?
 * - One cannot just simply cancel Future
 *   - cannot cancel on error condition
 *   - cannot cancel on race
 *
 * Futures will continue doing their work, spending precious computing resources, till the work is done
 *
 * Cancellable IO to the rescue!
 */

object FutureTimeout extends IOApp {

  import scala.concurrent.ExecutionContext.Implicits.global

  def runTask(i: Int): Future[Unit] =
    Future {
      blocking {
        (1 to i).foreach { iteration =>
          println(s"${Thread.currentThread().toString} Starting iteration - $iteration")
          Thread.sleep(1000)
          println(s"${Thread.currentThread().toString} Done iteration - $iteration")
        }
      }
    }

  def completeAfter(timeout: FiniteDuration): Future[Unit] =
    Future {
      blocking {
        Thread.sleep(timeout.toMillis)
        println(s"${Thread.currentThread().toString} Completing future after $timeout ")
      }
    }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO.fromFuture(
             IO.delay(
               Future.firstCompletedOf[Unit](
                 Seq(
                   runTask(10),
                   completeAfter(5.seconds)
                 )
               )
             )
           )
      //      _ <- IO.sleep(5.seconds)
    } yield ExitCode.Success
}

object IOTimeout extends IOApp {

  def runTask(i: Int): IO[Unit] =
    (1 to i).toList
      .map { iteration =>
        for {
          _ <- IO.delay(println(s"${Thread.currentThread().toString} Starting iteration - $iteration"))
          _ <- IO.sleep(1.second)
          _ <- IO.delay(println(s"${Thread.currentThread().toString} Done iteration - $iteration"))
        } yield ()
      }
      .sequence
      .void

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- runTask(10).timeout(5.seconds).attempt
      _ <- IO.delay(println(s"${Thread.currentThread().toString} Cancelled"))
      //      _ <- IO.sleep(5.seconds)
    } yield ExitCode.Success
}

object RaceApp extends IOApp {

  val tick: IO[Unit] =
    IO
      .delay(println("Working work long long never terminating"))
      .flatMap(_ => IO.sleep(1.second))
      .foreverM
      .void

  val workThatDoesFaster: IO[Unit] = IO.sleep(5.seconds) *> IO.delay(println("Work has been done"))

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO.race(tick, workThatDoesFaster).void
      //      _ <- IO.sleep(5.seconds)
      _ <- IO.delay(println("Terminating"))
    } yield ExitCode.Success
}

object SelfMadeTimeoutExercise extends IOApp {

  val tick: IO[Unit] =
    IO
      .delay(println("Working work long long never terminating"))
      .flatMap(_ => IO.sleep(1.second))
      .foreverM
      .void

  def timeoutIO[A](task: IO[A], timeout: FiniteDuration): IO[A] =
    IO.race(
      IO.sleep(timeout).as(new TimeoutException(s"Timeout of $timeout reached")),
      task
    ).flatMap(IO.fromEither)

  def tickF[F[_]: Sync: Timer]: F[Unit] =
    Sync[F]
      .delay(println("Working work long long never terminating"))
      .flatMap(_ => Timer[F].sleep(1.seconds))
      .foreverM
      .void

  def timeoutF[F[_]: Concurrent: Timer, A](task: F[A], timeout: FiniteDuration): F[A] =
    Concurrent[F].race(
      Timer[F].sleep(timeout).as(new TimeoutException(s"Timeout of $timeout reached")),
      task
    ).flatMap(Concurrent[F].fromEither)

  def run(args: List[String]): IO[ExitCode] =
    timeoutF(tickF[IO], 5.seconds)
      .handleErrorWith { e =>
        IO.delay(println(s"Failed with: ${e.getMessage}"))
      }
      .as(ExitCode.Success)
}

/* When writing your cancellable code, be aware that cancellation is a concurrent action.
 * That is, there is no synchronization provided by Cats Effect IO for it.
 * Therefore, if your effect code is doing an operation that isn't safe to do concurrently with cancellation,
 * it can lead data corruption or other errors.
 * You can solve it, for example, by introducing a lock, as per Cats Effect IO documentation here:
 * https://typelevel.org/cats-effect/docs/2.x/datatypes/io#gotcha-cancellation-is-a-concurrent-action
 */
object Cancellable extends IOApp {

  private val ec = Executors.newFixedThreadPool(4)

  class LegacyCode {

    private val cancelled = new AtomicBoolean(false)

    @tailrec
    private def longRecursiveCompute(x: Long, until: Long): Long =
      if (cancelled.get())
        throw new InterruptedException("longRecursiveCompute interrupted")
      else if (x >= until)
        x
      else {
        println(s" ${Thread.currentThread().getName} Calculating in longRecursiveCompute: $x")
        Thread.sleep(1000)
        longRecursiveCompute(x + x, until)
      }

    def compute(i: Long, until: Long)(onComplete: Long => Unit, onError: Exception => Unit): Unit = {
      val t = new Thread(() =>
        try onComplete(longRecursiveCompute(i, until))
        catch {
          case e: InterruptedException => onError(e)
        }
      )
      ec.execute(t)
    }

    def cancel(): Unit = cancelled.set(true)
  }

  override def run(args: List[String]): IO[ExitCode] = {

    val program = for {
      _     <- IO.delay(println("Launching cancelable"))
      io     = IO.cancelable[Long] { cb =>
                 val legacy = new LegacyCode
                 legacy.compute(2L, Long.MaxValue)(res => cb(Right(res)), e => cb(Left(e)))
//                 legacy.compute(2L, 100)(res => cb(Right(res)), e => cb(Left(e)))
                 IO.delay(legacy.cancel())
               }
      fiber <- io.start
      _     <- IO.delay(println(s"Started $fiber"))
      res   <- IO.race(IO.sleep(10.seconds), fiber.join)
      _     <- res.fold(
                 _ => IO.delay(println(s"cancelling $fiber...")) *> fiber.cancel *> IO.delay(println("IO cancelled")),
                 i => IO.delay(println(s"IO completed with: $i"))
               )
    } yield ExitCode.Success

    program.guarantee(IO(ec.shutdown()))
  }
}

/*
 * IO is cancellable only on async boundary `IO.shift` or on `IO.cancelBoundary` and after 512 flatMap loop iterations.
 * Documentation states:
 *
 *   We should also note that flatMap chains are only cancelable only if the chain happens after an asynchronous
 *   boundary.
 *
 *   After an asynchronous boundary, cancellation checks are performed on every N flatMap. The value of N is hardcoded to
 *   512.
 *
 * This is bit misleading, because cancellation is checked and counter IS reset on async boundary,
 * but the counter is still taken into account even if not crossing async boundaries.
 *
 * Technically IO is switching from Main to io-app context.
 *
 * That may lead to inconsistent state when doing `race` with internal state update.
 *
 * That means - critical blocks should be marked as `uncancellable`
 *
 * https://typelevel.org/cats-effect/docs/2.x/datatypes/io#concurrency-and-cancellation
 * https://typelevel.org/cats-effect/docs/2.x/datatypes/io#iocancelboundary
 */
object CancelBoundaries extends IOApp {

  val nonCancelableProgram: IO[Unit] = {

    //program has no context shift and no cancel boundary set, it's not cancellable
    def nonCancellableTimes(rec: Int): IO[Unit] =
      for {
        _ <- IO.delay(println(s"Running remaining iterations: $rec"))
        _ <- IO.sleep(1.seconds).uncancelable
        _ <- if (rec > 0) IO.suspend(nonCancellableTimes(rec - 1)) else IO.unit
      } yield ()

    for {
      _   <- IO.delay(println("Starting nonCancelableProgram"))
      fib <- nonCancellableTimes(10).start
      _   <- IO.sleep(5.seconds)
      _   <- fib.cancel
      _   <- IO.delay(println("Cancelled nonCancelableProgram"))
      _   <- IO.sleep(5.seconds) //just to keep program alive
      _   <- IO.delay(println("End nonCancelableProgram"))
    } yield ()
  }

  val cancelableProgram: IO[Unit] = {

    //on every iteration cancel boundary is set, program is cancellable
    def cancellableTimes(rec: Int): IO[Unit] =
      for {
        _ <- IO.delay(println(s"Running remaining iterations: $rec"))
        _ <- IO.sleep(1.seconds).uncancelable
        _ <- if (rec > 0) IO.cancelBoundary *> IO.suspend(cancellableTimes(rec - 1)) else IO.unit
      } yield ()

    for {
      _   <- IO.delay(println("Starting cancelableProgram"))
      fib <- cancellableTimes(10).start
      _   <- IO.sleep(5.seconds)
      _   <- fib.cancel
      _   <- IO.delay(println("Cancelled cancelableProgram"))
      _   <- IO.sleep(5.seconds) //just to keep program alive
      _   <- IO.delay(println("End cancelableProgram"))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- cancelableProgram
      _ <- nonCancelableProgram
    } yield ExitCode.Success
}

object CancelBoundariesExercise extends IOApp {

  def delay(duration: FiniteDuration): IO[Unit] = IO.sleep(duration).uncancelable

  /* Exercise #1
   * Fix retry function without altering delay function, to be cancellable immediately, so that running the program
   * there is no retrying after cancel
   */
  val retryExercise: IO[Unit] = {
    implicit class ioRetrySyntax[A](task: IO[A]) {
      def retry(
        id: String,
        maxRetries: Int,
        interval: FiniteDuration
      ): IO[A] =
        IO.cancelBoundary *> task
          .handleErrorWith {
            case NonFatal(e) =>
              IO
                .delay(println(s"$id Retrying... retries left: $maxRetries"))
                .flatMap { _ =>
                  if (maxRetries <= 0) IO.raiseError(e)
                  else
                    delay(interval) *> IO.suspend(
                      task.retry(
                        id,
                        maxRetries - 1,
                        interval
                      )
                    )
                }
          }
    }

    val io = IO.delay(if (Random.nextBoolean()) throw new RuntimeException("kaboom!") else "SUCCESS!")
    for {
      fib <- (0 to 10).toList
               .map(id => io.retry(s"id:$id", 10, 5.second))
               .parSequence
               .flatMap(ll => IO.delay(println(ll.toString())))
               .start
      _   <- IO.sleep(5.seconds)
      _   <- fib.cancel
      _   <- IO.delay(println("No more work after this point"))
      _   <- IO.sleep(10.seconds)
      _   <- IO.delay(println("End"))
    } yield ()
  }

  /* Exercise #2
   * Fix program so that no Calculation is happening after cancellation
   */
  val computeExercise: IO[Unit] = {

    def cpuBoundCompute(value: BigInt, multiplier: BigInt): IO[BigInt] = {
      val log = IO.delay(println(s"${Thread.currentThread().toString} Calculating... $multiplier"))
      IO.cancelBoundary *> log *> IO.suspend(cpuBoundCompute(value * multiplier, multiplier + 1))
    }

    for {
      _   <- IO.delay(println("Starting program"))
      fib <- cpuBoundCompute(1, 1).start
      _   <- fib.cancel
      _   <- IO.delay(println("cpuBoundCompute cancelled"))
      _   <- IO.sleep(10.seconds)
      _   <- IO.delay(println("End"))
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] =
    for {
//      _ <- retryExercise
      _ <- computeExercise
    } yield ExitCode.Success
}
