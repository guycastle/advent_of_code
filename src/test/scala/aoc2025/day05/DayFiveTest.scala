package aoc2025.day05

import utils.BaseTest

class DayFiveTest extends BaseTest:

  private val input: Seq[String] = """3-5
                                     |10-14
                                     |16-20
                                     |12-18
                                     |
                                     |1
                                     |5
                                     |8
                                     |11
                                     |17
                                     |32
                                     |""".stripMargin.split("\n").map(_.strip).toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayFive.partOne(input) mustBe 3

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayFive.partTwo(input) mustBe 0

end DayFiveTest
