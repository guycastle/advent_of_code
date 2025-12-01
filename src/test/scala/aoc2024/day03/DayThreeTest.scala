package aoc2024.day03

import utils.BaseTest
import utils.Syntax.*

class DayThreeTest extends BaseTest:

  private val inputPartOne: String = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  private val inputPartTwo: String = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  "Regex" must:

    "correctly parse all mul instructions" in:
      DayThree.mulInstructionRegex.findAllIn(inputPartOne).length mustBe 4

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayThree.partOne(inputPartOne.toSingleElementSeq) mustBe 161

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayThree.partTwo(inputPartTwo.toSingleElementSeq) mustBe 48

end DayThreeTest
