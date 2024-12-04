package aoc2024.day01

import utils.BaseTest

class DayOneTest extends BaseTest:

  val input: Seq[String] =
    """3   4
      |4   3
      |2   5
      |1   3
      |3   9
      |3   3""".stripMargin.split("\n").toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayOne.partOne(input) mustBe 11

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayOne.partTwo(input) mustBe 31

end DayOneTest
