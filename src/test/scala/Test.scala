import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.Comparator

import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class Test extends AnyFlatSpec with Matchers {
  def deleteRecursively(dir: Path): Unit = {
    Files.walk(dir)
      .sorted(Comparator.reverseOrder())
      .forEach(file => Files.deleteIfExists(file))
  }

  val test_path = Paths.get("./tmp")
  if (Files.exists(test_path)) deleteRecursively(test_path)
  val test_dir = Files.createDirectory(Paths.get("./tmp"))

  implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
  val program = new Program[Id, Path, Path]

  program.run(test_path)

  Files.exists(test_path.resolve("test_dir")) shouldBe true

  val f = test_path.resolve("test_dir/f")
  val b = test_path.resolve("test_dir/b")

  Files.exists(f) && Files.isDirectory(f) shouldBe true
  Files.exists(b) && Files.isDirectory(b) shouldBe true

  val filesNames = List("foo", "bar", "baz")

  for (name <- filesNames) {
    if (name.head == 'f')
      Files.exists(f.resolve(name)) && Files.isRegularFile(f.resolve(name)) shouldBe true
    else
      Files.exists(b.resolve(name)) && Files.isRegularFile(b.resolve(name)) shouldBe true
  }

  deleteRecursively(test_path)
}
