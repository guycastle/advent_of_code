package aoc2025.day02

import utils.BaseTest

class DayTwoTest extends BaseTest:

  private val input: Seq[String] =
    Seq("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

  "Parsed input in part one" must:

    "correctly identify sequences of digits repeating twice" in:
      DayTwo.isValidIdPart1(55) mustBe false
      DayTwo.isValidIdPart1(6464) mustBe false
      DayTwo.isValidIdPart1(123123) mustBe false
      DayTwo.isValidIdPart1(101) mustBe true
      DayTwo.isValidIdPart1(123456) mustBe true
      DayTwo.isValidIdPart1(111111) mustBe false
      DayTwo.isValidIdPart1(1001001) mustBe true

    "correctly process entire range" in:
      DayTwo.sumOfInvalidIds(11L to 22L, DayTwo.isValidIdPart1) mustBe 33
      DayTwo.sumOfInvalidIds(1188511880L to 1188511890L, DayTwo.isValidIdPart1) mustBe 1188511885

    "calculate the correct output" in:
      DayTwo.partOne(input) mustBe 1227775554L

  "Parsed input in part two" must:

    "correctly identify sequences of digits repeating twice" in:
      DayTwo.isValidIdPart2(1) mustBe true
      DayTwo.isValidIdPart2(55) mustBe false
      DayTwo.isValidIdPart2(6464) mustBe false
      DayTwo.isValidIdPart2(123123) mustBe false
      DayTwo.isValidIdPart2(101) mustBe true
      DayTwo.isValidIdPart2(123456) mustBe true
      DayTwo.isValidIdPart2(111111) mustBe false
      DayTwo.isValidIdPart2(1001001) mustBe true
      DayTwo.isValidIdPart2(111) mustBe false
      DayTwo.isValidIdPart2(123123123) mustBe false
      DayTwo.isValidIdPart2(1212121212) mustBe false
      DayTwo.isValidIdPart2(1111111) mustBe false
      DayTwo.isValidIdPart2(444555444555L) mustBe false

    "calculate the correct output" in:
      DayTwo.partTwo(input) mustBe 4174379265L

end DayTwoTest
