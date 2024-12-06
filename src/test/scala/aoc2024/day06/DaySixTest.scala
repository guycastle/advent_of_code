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

  "Processed input for part one" must :

    "return the correct distinct position count of the guard on the map" in:
      DaySix.partOne(input) mustBe 41

  "Processed input for part two" must :

    "return the correct number of potential obstruction positions that cause loops" in :
      DaySix.partTwo(input) mustBe 6

end DaySixTest
