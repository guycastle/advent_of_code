package aoc2025.day03

import utils.BaseTest

class DayThreeTest extends BaseTest:

  private val input: Seq[String] = """987654321111111
                                     |811111111111119
                                     |234234234234278
                                     |818181911112111""".stripMargin.split("\n").toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayThree.partOne(input) mustBe 357

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayThree.partTwo(input) mustBe 3121910778619L

end DayThreeTest
