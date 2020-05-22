package lab10

import cats.syntax.all._
import cats.effect.concurrent.MVar
import cats.effect.{ExitCode, IO, IOApp, Resource}
import scala.concurrent.duration._

object EchoCounter extends IOApp {
  def runPrinter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def printRec: IO[Unit] = for {
      value <- mvar.take
      _ <- IO(println(value))
      _ <- printRec
    } yield ()

    Resource.make(printRec.start)(_.cancel.flatMap(_ => IO(println("Printing ended")))).void
  }

  def runCounter(mvar: MVar[IO, String]): Resource[IO, Unit] = {
    def countRec(counter: Int): IO[Unit] = for {
      _ <- IO.sleep(1.seconds)
      _ <- mvar.put(counter.toString)
      _ <- countRec(counter + 1)
    } yield ()

    Resource.make(countRec(0).start)(_.cancel.flatMap(_ => IO(println("Counting ended")))).void
  }

  val program: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, String])(_ => IO.unit)
    _ <- runPrinter(mvar)
    _ <- runCounter(mvar)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    program.use(_ => IO.never)
}

