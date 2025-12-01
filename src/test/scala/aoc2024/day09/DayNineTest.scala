package aoc2024.day09

import utils.BaseTest
import utils.Syntax.*

class DayNineTest extends BaseTest:

  final private val input: String = "2333133121414131402"

  "Processing input for part one" must:

    "correctly calculate the filesystem checksum" in:
      DayNine.partOne(input.toSingleElementSeq) mustBe 1928

  "Processing input for part two" must:

    "correctly calculate the filesystem checksum after defragmentation by file blocks" in:
      DayNine.partTwo(input.toSingleElementSeq) mustBe 2858

end DayNineTest
