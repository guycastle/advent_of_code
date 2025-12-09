package aoc2025.day09

import utils.BaseTest

class DayNineTest extends BaseTest:

  private val input: Seq[String] = """7,1
                                     |11,1
                                     |11,7
                                     |9,7
                                     |9,5
                                     |2,5
                                     |2,3
                                     |7,3""".stripMargin.split("\n").toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayNine.partOne(input) mustBe 50

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayNine.partTwo(input) mustBe 24

end DayNineTest
