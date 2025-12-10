package aoc2025.day10

import utils.BaseTest

class DayTenTest extends BaseTest:

  private val input: Seq[String] = """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
                                     |[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
                                     |[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}""".stripMargin
    .split("\n")
    .toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayTen.partOne(input) mustBe 7

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayTen.partTwo(input) mustBe 0

end DayTenTest
