package aoc2024.day05

import utils.BaseTest

class DayFiveTest extends BaseTest:

  private val input: Seq[String] = """47|53
      |97|13
      |97|61
      |97|47
      |75|29
      |61|13
      |75|53
      |29|13
      |97|29
      |53|29
      |61|53
      |97|53
      |61|29
      |47|13
      |75|47
      |97|75
      |47|61
      |75|61
      |47|29
      |75|13
      |53|13
      |
      |75,47,61,53,29
      |97,61,53,29,13
      |75,29,13
      |75,97,47,61,53
      |61,13,29
      |97,13,75,29,47""".stripMargin.linesIterator.toSeq

  "Processing input for part one" must:

    "return the sum of the correctly ordered updates' middle pages" in:
      DayFive.partOne(input) mustBe 143

  "Processing input for part two" must:

    "return the sum of the incorrectly ordered updates' middle pages after applying rules" in:
      DayFive.partTwo(input) mustBe 123

end DayFiveTest
