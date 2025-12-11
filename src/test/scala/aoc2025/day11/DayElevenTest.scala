package aoc2025.day11

import utils.BaseTest

class DayElevenTest extends BaseTest:

  private val input: Seq[String] = """aaa: you hhh
                                     |you: bbb ccc
                                     |bbb: ddd eee
                                     |ccc: ddd eee fff
                                     |ddd: ggg
                                     |eee: out
                                     |fff: out
                                     |ggg: out
                                     |hhh: ccc fff iii
                                     |iii: out""".stripMargin.split("\n").toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayEleven.partOne(input) mustBe 5

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayEleven.partTwo(input) mustBe 0

end DayElevenTest
