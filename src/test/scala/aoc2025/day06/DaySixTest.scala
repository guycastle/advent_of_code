package aoc2025.day06

import utils.BaseTest

class DaySixTest extends BaseTest:

  private val input: Seq[String] = """123 328  51 64 
                                     | 45 64  387 23 
                                     |  6 98  215 314
                                     |*   +   *   +  
                                     |""".stripMargin.split("\n").map(_.strip).toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DaySix.partOne(input) mustBe 4277556

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DaySix.partTwo(input) mustBe 0

end DaySixTest
