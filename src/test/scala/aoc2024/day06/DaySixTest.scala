package aoc2024.day06

import utils.BaseTest

class DaySixTest extends BaseTest:

  private val input: Seq[String] =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin.linesIterator.toSeq

  "Parsed input for part one" must:

    "return the correct distinct position count of the guard on the map" in:
      DaySix.partOne(input) mustBe 41
