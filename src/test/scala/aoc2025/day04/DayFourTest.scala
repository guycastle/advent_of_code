package aoc2025.day04

import utils.BaseTest

class DayFourTest extends BaseTest:

  private val input: Seq[String] = """..@@.@@@@.
                                     |@@@.@.@.@@
                                     |@@@@@.@.@@
                                     |@.@@@@..@.
                                     |@@.@@@@.@@
                                     |.@@@@@@@.@
                                     |.@.@.@.@@@
                                     |@.@@@.@@@@
                                     |.@@@@@@@@.
                                     |@.@.@@@.@.
                                     |""".stripMargin.split("\n").map(_.strip).filterNot(_.isBlank).toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayFour.partOne(input) mustBe 13

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayFour.partTwo(input) mustBe 43

end DayFourTest
