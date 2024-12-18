package aoc2023.day13

import utils.BaseTest

class DayThirteenTest extends BaseTest:

  lazy val input: Seq[String] = Seq(
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#",
  )

  lazy val edgeCase1: Seq[String] = Seq(
    "#.#.####.#.#.##",
    ".#.##..##.#...#",
    ".#..#..#..#.#..",
    ".##..##..##.#.#",
    "....####.......",
    "##...##...##..#",
    "##...##...##..#",
    "....####.......",
    ".##..##..##.#.#",
    ".#..#..#..#.#..",
    ".#.##..##.#...#",
    "#.#.####.#.#.#.",
    "..#.#..#.#....#",
    "##.#.##.#.####.",
    "..#..##..#....#",
    "#..#.##.#..#.##",
    "#.#.####.#.#...",
  )

  lazy val edgeCase2: Seq[String] = Seq(
    "##.#.##...####.",
    "#####..#...##..",
    "##.##.#.#####.#",
    ".##.#..##..##..",
    "###..#.#####.#.",
    "###..#.#####.#.",
    ".##.#..##..##..",
    "##.##.#.#####.#",
    "#####..#...##..",
    "##.#.##..#####.",
    "...#...#....###",
    "##.#.###.###..#",
    "..#.###.#.#....",
    "####...#.#.#...",
    ".#..#....##.#.#",
    "....####.###.##",
    "....####.###.##",
  )

  "Day thirteen's solution" must:

    "parse the input patterns correctly" in:
      val patterns = DayThirteen.parseInput(input)
      patterns.size mustBe 2
      patterns.headOption.value.size mustBe 7
      patterns.lastOption.value.size mustBe 7

    "detect the correct index at which the pattern mirrors" in:
      val indices = DayThirteen.parseInput(input).map(DayThirteen.reflectionIndex(_))
      indices.headOption.value mustBe 5
      indices.lastOption.value mustBe 400

    "correctly determine the index for edge cases" in:
      DayThirteen.parseInput(edgeCase1).map(DayThirteen.reflectionIndex(_)).headOption.value mustBe 6
      DayThirteen.parseInput(edgeCase2).map(DayThirteen.reflectionIndex(_)).headOption.value mustBe 1600

    "calculate the correct value for part one" in:
      DayThirteen.partOne(input) mustBe 405

    "calculate the correct value for part two" in:
      DayThirteen.partTwo(input) mustBe 400

end DayThirteenTest
