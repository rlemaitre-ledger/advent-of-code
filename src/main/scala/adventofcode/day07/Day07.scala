package adventofcode.day07

import adventofcode.AdventOfCodeBase
import scala.collection.immutable.List
import scala.util.matching.Regex

object Day07 extends AdventOfCodeBase[Long]("day07.txt") {
  def parse(lines: List[String]): List[TerminalOutput] =
    lines.flatMap { line =>
      if (line.matches(Command.pattern.toString())) {
        List(Command.from(line))
      } else if (line.matches(LsOutput.pattern.toString())) {
        List(LsOutput.from(line))
      } else {
        Nil
      }
    }
  def fileSystem(commands: List[TerminalOutput]): FileSystem =
    commands.foldLeft(FileSystem.empty) { case (fs: FileSystem, output: TerminalOutput) =>
      output match
        case cd(dir)      => fs.moveTo(dir)
        case LsDir(name)  => fs.addDir(name)
        case file: LsFile => fs.addFile(file.toFile)
        case _            => fs
    }

  def system(lines: List[String]): FileSystem = fileSystem(parse(lines))

  override def part1(lines: List[String]): Long = {
    val fs: FileSystem = system(lines)
    fs.directories.map { case (path, _) => fs.totalSize(path) }.filter(_ < 100000L).sum
  }

  override def part2(lines: List[String]): Long = {
    val fs: FileSystem = system(lines)
    val needed         = fs.needed(updateSize)
    fs.directories
      .map { case (path, dir) => (dir, fs.totalSize(path)) }
      .toList
      .filter(_._2 >= needed)
      .map(_._2)
      .min
  }
}

sealed trait TerminalOutput
sealed trait Command       extends TerminalOutput
case object ls             extends Command
case class cd(arg: String) extends Command
object Command {
  def from(input: String): Command = input match
    case pattern(cmd, arg: String) =>
      cmd match
        case "cd" => cd(arg)
        case "ls" => ls
  val pattern: Regex = """\$ (cd|ls) ?(.*)""".r

}

sealed trait LsOutput(name: String) extends TerminalOutput

case class LsDir(name: String) extends LsOutput(name) {}
case class LsFile(name: String, size: Long) extends LsOutput(name) {
  def toFile: File = File(name, size)
}
object LsOutput {
  def from(input: String): LsOutput = input match
    case pattern(start, fileName) =>
      if (start == "dir") {
        LsDir(fileName)
      } else {
        LsFile(fileName, start.toLong)
      }
  val pattern: Regex = """([0-9]+|dir) (.*)""".r
}

sealed trait FsEntry(name: String) {
  def size: Long
}

case class Directory(name: String, path: List[String], directFiles: Map[String, File]) extends FsEntry(name) {
  val size: Long = directFiles.values.foldLeft(0L)(_ + _.size)
}

case class File(name: String, size: Long) extends FsEntry(name)

case class FileSystem(currentPath: List[String], directories: Map[List[String], Directory]) {
  def moveTo(dir: String): FileSystem = dir match
    case "/"  => copy(currentPath = List(""))
    case ".." => copy(currentPath = currentPath.init)
    case dir  => addDir(dir).copy(currentPath = currentPath :+ dir)

  def addDir(name: String): FileSystem =
    copy(directories = directories + ((currentPath :+ name) -> Directory(name, currentPath, Map.empty)))
  def addFile(file: File): FileSystem = {
    val directory = directories(currentPath)
    val withFile  = directory.copy(directFiles = directory.directFiles + (file.name -> file))
    copy(directories = directories + (currentPath -> withFile))
  }

  def totalSize(path: List[String]): Long = directories.filter(_._1.startsWith(path)).values.map(_.size).sum

  def used: Long = totalSize(List(""))

  def freeSpace: Long = FileSystem.totalSize - used

  def needed(size: Long): Long = (size - freeSpace) max 0
}
object FileSystem {
  val totalSize: Long = 70000000L
  val empty: FileSystem =
    FileSystem(
      Nil,
      Map(
        List("") -> Directory("/", List(), Map.empty)
      )
    )
}

val updateSize = 30_000_000L