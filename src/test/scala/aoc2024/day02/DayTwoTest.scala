package aoc2024.day02

import utils.BaseTest

class DayTwoTest extends BaseTest:

  private val input: Seq[String] =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9""".stripMargin.linesIterator.toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayTwo.partOne(input) mustBe 2

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayTwo.partTwo(input) mustBe 4

end DayTwoTest
