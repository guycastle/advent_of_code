package aoc2025.day02

import utils.BaseTest

class DayTwoTest extends BaseTest:

  private val input: Seq[String] =
    Seq("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

  "Processing range" must:
    "correctly identify sequences of digits repeating twice" in:
      DayTwo.isValidId(55) mustBe false
      DayTwo.isValidId(6464) mustBe false
      DayTwo.isValidId(123123) mustBe false
      DayTwo.isValidId(101) mustBe true
      DayTwo.isValidId(123456) mustBe true
      DayTwo.isValidId(111111) mustBe false
      DayTwo.isValidId(1001001) mustBe true

    "correctly process entire range" in:
      DayTwo.sumOfInvalidIds(11L to 22L) mustBe 33
      DayTwo.sumOfInvalidIds(1188511880L to 1188511890L) mustBe 1188511885

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayTwo.partOne(input) mustBe 1227775554

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayTwo.partTwo(input) mustBe 0

end DayTwoTest
