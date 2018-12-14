package race

import cats.data._
import cats.effect._
import cats.implicits._

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

    def :+(t: Throwable): CompositeException = copy(ex = ex :+ t)
  }

  // And implement this function:
  def raceToSuccess[F[_], A](fs: NonEmptyList[F[A]])
                            (implicit F: Concurrent[F]): F[A] = {
    val head = fs.head.attempt.map {
      case Left(t)  => Left(CompositeException(NonEmptyList.one(t)))
      case Right(a) => Right(a)
    }

    val raceResult = fs.tail.foldLeft(head) { (acc, proc) =>
      F.racePair(acc, proc.attempt).flatMap {
        case Left((Left(comp), procFib)) => procFib.join.map(_.leftMap(comp :+ _))
        case Right((accFib, Left(t)))    => accFib.join.map(_.leftMap(_ :+ t))

        case Left((Right(a), procFib)) => cancel(procFib).map(_ => Right(a))
        case Right((accFib, Right(a))) => cancel(accFib).map(_ => Right(a))
      }
    }

    raceResult.map {
      case Left(comp) => throw comp
      case Right(a)   => a
    }
  }

  /** fiber.cancel returns a CancelToken[F], which is an alias for F[Unit].
    * IntelliJ doesn't cope well resolving type classes for this alias, so
    * this method exists to "unalias" the cancel method.
    */
  private def cancel[F[_]](fiber: Fiber[F, _]): F[Unit] = fiber.cancel

  // In your IOApp, you can use the following sample method list
  def methods[F[_] : Sync : Timer]: NonEmptyList[F[Data]] = NonEmptyList.of(
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

