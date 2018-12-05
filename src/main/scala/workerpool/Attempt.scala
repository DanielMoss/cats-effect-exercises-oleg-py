package workerpool

import cats.effect.concurrent.{MVar, Ref}
import cats.effect._
import cats.implicits._

import scala.concurrent.duration._
import scala.language.higherKinds
import scala.util.Random

object Attempt extends IOApp {
  // To start, our requests can be modelled as simple functions.
  // You might want to replace this type with a class if you go for bonuses. Or not.
  type Worker[F[_], A, B] = A => F[B]

  // Sample stateful worker that keeps count of requests it has accepted
  def mkWorker[F[_]](id: Int)
                    (implicit timer: Timer[F], F: Concurrent[F]): F[Worker[F, Int, Int]] =
    Ref[F].of(0).map { counter =>
      def simulateWork: F[Unit] =
        F.delay(50 + Random.nextInt(450)).map(_.millis).flatMap(timer.sleep)

      def report: F[Unit] =
        counter.get.flatMap(i => F.delay(println(s"Total processed by $id: $i")))

      x => simulateWork >>
        counter.update(_ + 1) >>
        report >>
        F.pure(x + 1)
    }

  trait WorkerPool[F[_], A, B] {
    def exec(a: A): F[B]
    def add(worker: Worker[F, A, B]): F[Unit]
    def removeAllWorkers(): F[Unit]
  }

  object WorkerPool {
    // Implement this constructor, and, correspondingly, the interface above.
    // You are free to use named or anonymous classes
    def of[F[_]]: OfPartiallyApplied[F] = new OfPartiallyApplied[F]

    final class OfPartiallyApplied[F[_]](val dummy: Boolean = true) extends AnyVal {
      def apply[A, B](fs: List[Worker[F, A, B]])(implicit F: Concurrent[F]): F[WorkerPool[F, A, B]] =
        for {
          nextWorker     <- asyncPutAll[F, Worker[F, A, B]](MVar[F].empty, fs)
          currentWorkers <- Ref[F].of(fs)
        } yield poolImpl[F, A, B](currentWorkers, nextWorker)
    }

    /** [[MVar.put]], but we do not wait for the operation to complete */
    private def asyncPut[F[_], T](mVar: MVar[F, T], t: T)
                                 (implicit F: Concurrent[F]): F[Unit] =
      F.start(mVar.put(t)) >> F.pure(())

    /** [[asyncPut]], but we queue up all elements of the list into the MVar */
    private def asyncPutAll[F[_] : Concurrent, T](initialMVar: F[MVar[F, T]], ts: List[T]): F[MVar[F, T]] =
      ts.foldLeft(initialMVar)((fMVar, t) =>
        for {
          mVar <- fMVar
          _    <- asyncPut(mVar, t)
        } yield mVar
      )

    private def poolImpl[F[_], A, B](currentWorkers: Ref[F, List[Worker[F, A, B]]],
                                     nextWorker: MVar[F, Worker[F, A, B]])
                                    (implicit F: Concurrent[F]): WorkerPool[F, A, B] =
      new WorkerPool[F, A, B] {
        def exec(a: A): F[B] =
          acquireWorker.flatMap(worker =>
            for {
              result <- worker.apply(a)
              _      <- F.start(release(worker))
            } yield result
          )

        def add(worker: Worker[F, A, B]): F[Unit] =
          currentWorkers.update(_ :+ worker) >> asyncPut(nextWorker, worker)

        def removeAllWorkers(): F[Unit] =
          currentWorkers.set(List.empty)

        private def acquireWorker: F[Worker[F, A, B]] =
          for {
            maybeWorker <- nextWorker.take
            workers     <- currentWorkers.get
            worker      <- if (workers.contains(maybeWorker)) F.pure(maybeWorker) else acquireWorker
          } yield worker

        private def release(worker: Worker[F, A, B]): F[Unit] =
          currentWorkers.get.flatMap(workers =>
            if (workers.contains(worker)) asyncPut(nextWorker, worker) else F.pure(())
          )
      }
  }

  // Sample test pool to play with in IOApp
  val testPool: IO[WorkerPool[IO, Int, Int]] =
    List.range(0, 2)
      .traverse(mkWorker[IO])
      .flatMap(WorkerPool.of[IO](_))

  def run(args: List[String]): IO[ExitCode] =
    for {
      pool <- testPool
      _    <- pool.exec(42).replicateA(20)
    } yield ExitCode.Success
}
