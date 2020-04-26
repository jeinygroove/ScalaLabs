import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import cats.instances.list._
import cats.{Applicative, Id, Monad}
import cats.syntax.all._

import scala.jdk.CollectionConverters._
import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait getNames[F[_], File] {
  def getNames(file: List[File]): F[List[String]]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(dir: Dir, file: File): F[File]
}

trait getDirFiles[F[_], Dir, File] {
  def getDirFiles(dir: Dir): F[List[File]]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               getDirFiles: getDirFiles[F, Dir, File],
                               getNames: getNames[F, File],
                               moveFile: MoveFile[F, Dir, File],
                               printer: Printer[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    test_dir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(test_dir, "foo")
    _ <- mkFile.mkFile(test_dir, "bar")
    _ <- mkFile.mkFile(test_dir, "baz")
    files <- getDirFiles.getDirFiles(test_dir)
    _ <- files.map(fileName => printer.printName(fileName)).sequence
    names <- getNames.getNames(files)
    first_chars = names.map(name => name.head)
    new_dirs <- first_chars.map (ch => mkDir.mkDir(test_dir, ch.toString)).sequence
    _ <- files.zip(new_dirs).map(p => moveFile.moveFile(p._2, p._1)).sequence
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path]
  with getDirFiles[F, Path, Path] with MoveFile[F, Path, Path] with getNames[F, Path] {
  override def mkDir(dir: Path, name: String): F[Path] = {
    if (!Files.exists(dir.resolve(name))) {
      Files.createDirectory(dir.resolve(name)).pure[F]
    } else
      dir.resolve(name).pure[F]
  }

  override def mkFile(dir: Path, name: String): F[Path] = {
    if (!Files.exists(dir.resolve(name)))
      Files.createFile(dir.resolve(name)).pure[F]
    else
      dir.resolve(name).pure[F]
  }

  override def getDirFiles(dir: Path): F[List[Path]] = {
    Files.list(dir).filter(Files.isRegularFile(_)).iterator().asScala.toList.pure[F]
  }

  override def moveFile(dir: Path, file: Path): F[Path] = {
    Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
  }

  override def getNames(files: List[Path]): F[List[String]] = {
    files.map(path => path.getFileName.toString).pure[F]
  }
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]

    program.run(Paths.get("."))
  }
}
