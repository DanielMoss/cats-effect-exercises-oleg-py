package race

import cats.data._
import cats.effect._
import cats.implicits._

import scala.concurrent.duration._
import scala.util.Random

object Definition extends IOApp {
  case class Data(source: String, body: String)

  def provider(name: String)(implicit timer: Timer[IO]): IO[Data] = {
    val proc = for {
      dur <- IO { Random.nextInt(500) }
      _ <- IO.sleep { (100 + dur).millis }
      _ <- IO { if (Random.nextBoolean()) throw new Exception(s"Error in $name") }
      txt <- IO { Random.alphanumeric.take(16).mkString }
    } yield Data(name, txt)

    proc.guaranteeCase {
      case ExitCase.Completed => IO { println(s"$name request finished") }
      case ExitCase.Canceled  => IO { println(s"$name request canceled") }
      case ExitCase.Error(ex) => IO { println(s"$name errored") }
    }
  }

  // Use this class for reporting all failures.
  case class CompositeException(ex: NonEmptyList[Throwable]) extends Exception("All race candidates have failed")

  // And implement this function:
  def raceToSuccess[A](ios: NonEmptyList[IO[A]]): IO[A] = ???

  // In your IOApp, you can use the following sample method list

  val methods: NonEmptyList[IO[Data]] = NonEmptyList.of(
    "memcached",
    "redis",
    "postgres",
    "mongodb",
    "hdd",
    "aws"
  )
    .map(provider)

  def run(args: List[String]): IO[ExitCode] = {
    def oneRace = raceToSuccess(methods)
      .flatMap(a => IO(println(s"Final result is $a")))
      .handleErrorWith(err => IO(err.printStackTrace()))

    oneRace.replicateA(5).as(ExitCode.Success)
  }
}

