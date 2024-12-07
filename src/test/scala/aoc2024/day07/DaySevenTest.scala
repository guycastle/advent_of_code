package aoc2024.day07

import utils.BaseTest

class DaySevenTest extends BaseTest:

  private final val input: Seq[String] =
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20""".stripMargin.linesIterator.toSeq

  "Processing the input for part one" must:

    "Return the sum of the calibration of equations that can be true" in:
      DaySeven.partOne(input) mustBe 3749

  "Processing the input for part two" must :

    "Take into account the extra concatenation operator and return the sum like in part one" in :
      DaySeven.partTwo(input) mustBe 11387
end DaySevenTest
