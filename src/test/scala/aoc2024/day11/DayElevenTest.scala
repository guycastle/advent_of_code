package aoc2024.day11

import utils.BaseTest
import utils.Syntax.*

class DayElevenTest extends BaseTest:

  private final val input: Seq[String] = "125 17".toSingleElementSeq

  "Processing the input for part one" must:

    "correctly calculate the number of stones after 25 blinks" in:
      DayEleven.partOne(input) mustBe 55312

end DayElevenTest
