package aoc2024.day04

import utils.BaseTest

class DayFourTest extends BaseTest:

  private val input: Seq[String] =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin.linesIterator.toSeq

  private val edgeCase1: Seq[String] =
    """SXSXXXX
      |XAXXXXX
      |MXMXXXX
      |XAXXXXX
      |SXSXXXX
      |SXMXSXM
      |XAXAXAM
      |SXMXSXM""".stripMargin.linesIterator.toSeq

  private val edgeCase2: Seq[String] =
    """MXMXMXMXS
      |XAXAXASAX
      |SXSXSXMXS""".stripMargin.linesIterator.toSeq

  "Processing input for part one" must:

    "return the correct count of XMAS instances" in:
      DayFour.partOne(input) mustBe 18

  "Processing input for part two" must:

    "return the correct count of crossed MAS instances" in:
      DayFour.partTwo(input) mustBe 9

    "return the correct count of crossed MAS instances for the edge cases" in:
      DayFour.partTwo(edgeCase1) mustBe 5
      DayFour.partTwo(edgeCase2) mustBe 3

end DayFourTest
