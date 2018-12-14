package race

import cats.data._
import cats.effect._
import cats.implicits._
import cats.{Functor, Reducible}

import scala.concurrent.duration._
import scala.language.higherKinds
import scala.util.Random

object Attempt extends IOApp {
  final case class Data(source: String, body: String)

  def provider[F[_]](name: String)
                    (implicit F: Sync[F], timer: Timer[F]): F[Data] = {
    val proc = for {
      dur <- F.delay(Random.nextInt(500))
      _   <- timer.sleep((100 + dur).millis)
      _   <- F.delay(if (Random.nextBoolean()) throw new Exception(s"Error in $name"))
      txt <- F.delay(Random.alphanumeric.take(16).mkString)
    } yield Data(name, txt)

    F.guaranteeCase(proc) {
      case ExitCase.Completed => F.delay(println(s"$name request finished"))
      case ExitCase.Canceled  => F.delay(println(s"$name request canceled"))
      case ExitCase.Error(_)  => F.delay(println(s"$name errored"))
    }
  }

  // Use this class for reporting all failures.
  final case class CompositeException(ex: NonEmptyList[Throwable]) extends Exception("All race candidates have failed") {
    override def getMessage: String =
      s"${super.getMessage}: ${ex.map(_.getMessage)}"
  }

  object CompositeException {
    def apply(t1: Throwable, t2: Throwable): CompositeException =
      (t1, t2) match {
        case (CompositeException(l1), CompositeException(l2)) => CompositeException(l1 combine l2)
        case (CompositeException(l),  _)                      => CompositeException(l :+ t2)
        case (_,                      CompositeException(l))  => CompositeException(l :+ t1)
        case (_,                      _)                      => CompositeException(NonEmptyList.of(t1, t2))
      }
  }

  // And implement this function:
  def raceToSuccess[F[_], T[_] : Reducible, A](fs: T[F[A]])
                                              (implicit F: Concurrent[F]): F[A] =
    fs.reduce { (f1, f2) =>
      F.racePair(f1.attempt, f2.attempt).flatMap {
        case Left((Left(t), f2Fib))  => composeErrorsAndContinue(t, f2Fib)
        case Right((f1Fib, Left(t))) => composeErrorsAndContinue(t, f1Fib)

        case Left((Right(a), f2Fib))  => cancelAndReturnResult(a, f2Fib)
        case Right((f1Fib, Right(a))) => cancelAndReturnResult(a, f1Fib)
      }
    }

  private def composeErrorsAndContinue[F[_] : Functor, A](t: Throwable, fiber: Fiber[F, Either[Throwable, A]]): F[A] =
    fiber.join.map {
      case Left(t2) => throw CompositeException(t, t2)
      case Right(a) => a
    }

  private def cancelAndReturnResult[F[_] : Functor, A](result: A, fiber: Fiber[F, _]): F[A] = {
    // fiber.cancel returns a CancelToken[F], which is an alias for F[Unit].
    // IntelliJ doesn't cope well resolving type classes for this alias, so
    // this method exists to "unalias" the cancel method.
    val cancelledFiber: F[Unit] = fiber.cancel
    cancelledFiber.map(_ => result)
  }

  // In your IOApp, you can use the following sample method list
  def methods[F[_] : Sync : Timer]: NonEmptyVector[F[Data]] = NonEmptyVector.of(
    "memcached",
    "redis",
    "postgres",
    "mongodb",
    "hdd",
    "aws"
  ).map(provider[F])

  def run(args: List[String]): IO[ExitCode] = {
    def oneRace: IO[Unit] = raceToSuccess(methods[IO])
      .flatMap(a => IO.delay(println(s"Final result is $a")))
      .handleErrorWith(err => IO.delay(err.printStackTrace()))

    oneRace.replicateA(5).as(ExitCode.Success)
  }
}

