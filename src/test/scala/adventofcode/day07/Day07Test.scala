package adventofcode.day07

import adventofcode.AoCTest
import adventofcode.Mode
import adventofcode.day07.Day07.*

class Day07Test extends AoCTest {
  override val lines: String = """$ cd /
                                 |$ ls
                                 |dir a
                                 |14848514 b.txt
                                 |8504156 c.dat
                                 |dir d
                                 |$ cd a
                                 |$ ls
                                 |dir e
                                 |29116 f
                                 |2557 g
                                 |62596 h.lst
                                 |$ cd e
                                 |$ ls
                                 |584 i
                                 |$ cd ..
                                 |$ cd ..
                                 |$ cd d
                                 |$ ls
                                 |4060174 j
                                 |8033020 d.log
                                 |5626152 d.ext
                                 |7214296 k""".stripMargin

  val testInstance: Day07 = Day07(parse(input))

  test("parse filesystem entries") {
    assertEquals(LsOutput.from("dir a"), LsDir("a"))
    assertEquals(LsOutput.from("14848514 b.txt"), LsFile("b.txt", 14848514L))
    assertEquals(LsOutput.from("8504156 c.dat"), LsFile("c.dat", 8504156L))
  }
  test("parse command") {
    assertEquals(Command.from("$ cd /"), cd("/"))
    assertEquals(Command.from("$ cd d"), cd("d"))
    assertEquals(Command.from("$ cd .."), cd(".."))
    assertEquals(Command.from("$ ls"), ls)
  }
  test("parse input") {
    assertEquals(
      Day07.parse(input),
      List(
        cd("/"),
        ls,
        LsDir("a"),
        LsFile("b.txt", 14848514L),
        LsFile("c.dat", 8504156L),
        LsDir("d"),
        cd("a"),
        ls,
        LsDir("e"),
        LsFile("f", 29116L),
        LsFile("g", 2557L),
        LsFile("h.lst", 62596L),
        cd("e"),
        ls,
        LsFile("i", 584L),
        cd(".."),
        cd(".."),
        cd("d"),
        ls,
        LsFile("j", 4060174L),
        LsFile("d.log", 8033020L),
        LsFile("d.ext", 5626152L),
        LsFile("k", 7214296L)
      )
    )
    assertEquals(Day07.parse(List("sdfghjrty")), Nil)
  }
  test("part 1") {
    assertEquals(testInstance.part1, 95437L)
  }
  test("directory size") {
    val subSubDir = Directory("c", List("", "a", "b"), Map("c1" -> File("c1", 1L)))
    val subDir    = Directory("b", List("", "a"), Map("b1" -> File("b1", 1L)))
    val dir       = Directory("a", List(""), Map("a1" -> File("a1", 1L)))
    val root      = Directory("a", List(), Map.empty)
    val dirs = Map(
      List("")                -> root,
      List("", "a")           -> dir,
      List("", "a", "b")      -> subDir,
      List("", "a", "b", "c") -> subSubDir
    )
    val fs = FileSystem(List(""), dirs)
    assertEquals(fs.totalSize(List("", "a")), 3L)
    assertEquals(fs.totalSize(List("", "a", "b")), 2L)
    assertEquals(fs.totalSize(List("", "a", "b", "c")), 1L)
  }
  test("used size") {
    assertEquals(testInstance.fileSystem.used, 48381165L)
    assertEquals(testInstance.fileSystem.freeSpace, 21618835L)
    assertEquals(testInstance.fileSystem.needed(updateSize), 8381165L)
  }
  test("part 2") {
    assertEquals(testInstance.part2, 24933642L)
  }
  test("answers") {
    assertEquals(Day07.instance.run(Mode.Part1), 1315285L)
    assertEquals(Day07.instance.run(Mode.Part2), 9847279L)
  }
}
