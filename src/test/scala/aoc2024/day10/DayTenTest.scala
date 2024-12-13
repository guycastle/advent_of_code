package aoc2024.day10

import utils.BaseTest

class DayTenTest extends BaseTest:

  private final val input: Seq[String] =
    """89010123
      |78121874
      |87430965
      |96549874
      |45678903
      |32019012
      |01329801
      |10456732""".stripMargin.linesIterator.toSeq

//  "Processing the input for part one" must:
//
//    "correctly calculate the sum of the scores of all trailheads on the map" in:
//      DayTen.partOne(input) mustBe 36
  
  "Processing the input for part two" must:
    
      "correctly calculate the sum of the ratings of all trailheads on the map" in:
        DayTen.partTwo(input) mustBe 81

end DayTenTest
