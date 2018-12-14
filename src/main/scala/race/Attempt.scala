package race

import cats.data._
import cats.effect._
import cats.implicits._

import scala.concurrent.duration._
import scala.util.Random

object Attempt extends IOApp {
  final case class Data(source: String, body: String)

  def provider(name: String)(implicit timer: Timer[IO]): IO[Data] = {
    val proc = for {
      dur <- IO.delay(Random.nextInt(500))
      _   <- IO.sleep((100 + dur).millis)
      _   <- IO.delay(if (Random.nextBoolean()) throw new Exception(s"Error in $name"))
      txt <- IO.delay(Random.alphanumeric.take(16).mkString)
    } yield Data(name, txt)

    proc.guaranteeCase {
      case ExitCase.Completed => IO.delay(println(s"$name request finished"))
      case ExitCase.Canceled  => IO.delay(println(s"$name request canceled"))
      case ExitCase.Error(_)  => IO.delay(println(s"$name errored"))
    }
  }

  // Use this class for reporting all failures.
  final case class CompositeException(ex: NonEmptyList[Throwable]) extends Exception("All race candidates have failed") {
    override def getMessage: String =
      s"${super.getMessage}: ${ex.map(_.getMessage)}"

    def :+(t: Throwable): CompositeException = copy(ex = ex :+ t)
  }

  // And implement this function:
  def raceToSuccess[A](ios: NonEmptyList[IO[A]]): IO[A] = {
    val head = ios.head.redeem(
      t => Left(CompositeException(NonEmptyList.one(t))),
      a => Right(a)
    )

    val raceResult = ios.tail.foldLeft(head) { (acc, proc) =>
      IO.racePair(acc, proc.attempt).flatMap {
        case Left((Left(comp), procFib)) => procFib.join.map(_.leftMap(comp :+ _))
        case Right((accFib, Left(t)))    => accFib.join.map(_.leftMap(_ :+ t))

        case Left((Right(a), procFib)) => procFib.cancel.map(_ => Right(a))
        case Right((accFib, Right(a))) => accFib.cancel.map(_ => Right(a))
      }
    }

    raceResult.map {
      case Left(comp) => throw comp
      case Right(a)   => a
    }
  }

  // In your IOApp, you can use the following sample method list
  val methods: NonEmptyList[IO[Data]] = NonEmptyList.of(
    "memcached",
    "redis",
    "postgres",
    "mongodb",
    "hdd",
    "aws"
  ).map(provider)

  def run(args: List[String]): IO[ExitCode] = {
    def oneRace: IO[Unit] = raceToSuccess(methods)
      .flatMap(a => IO.delay(println(s"Final result is $a")))
      .handleErrorWith(err => IO.delay(err.printStackTrace()))

    oneRace.replicateA(5).as(ExitCode.Success)
  }
}

