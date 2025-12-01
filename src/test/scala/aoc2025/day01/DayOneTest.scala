package aoc2025.day01

import aoc2025.day01.DayOne.Rotation
import utils.BaseTest

class DayOneTest extends BaseTest:

  private val input: Seq[String] = """L68
      |L30
      |R48
      |L5
      |R60
      |L55
      |L1
      |L99
      |R14
      |L82""".stripMargin.split("\n").toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayOne.partOne(input) mustBe 3

  "Rotation calculation" must:
    "correctly calculate the number of complete rotations and new position" in:
      val result1 = Rotation.left(3).rotate(3)
      result1.position mustBe 0
      result1.completeRotations mustBe 1
      val result2 = Rotation.right(3).rotate(result1.position)
      result2.position mustBe 3
      result2.completeRotations mustBe 0
      val result3 = Rotation.left(4).rotate(result2.position)
      result3.position mustBe 99
      result3.completeRotations mustBe 1
      val result4 = Rotation.right(1001).rotate(result3.position)
      result4.position mustBe 0
      result4.completeRotations mustBe 11
      val result5 = Rotation.left(5).rotate(result4.position)
      result5.position mustBe 95
      result5.completeRotations mustBe 0

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayOne.partTwo(input) mustBe 6

end DayOneTest
