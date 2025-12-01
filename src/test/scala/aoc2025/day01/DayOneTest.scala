package aoc2025.day01

import utils.BaseTest

class DayOneTest extends BaseTest:

  private val input: Seq[String] =
    """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82""".stripMargin.split("\n").toIndexedSeq

  "Parsed input in part one" must :

    "calculate the correct output" in :
      DayOne.partOne(input) mustBe 3

  "Parsed input in part two" must :

    "calculate the correct output" in :
      DayOne.partTwo(input) mustBe 0

end DayOneTest
