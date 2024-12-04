package aoc2023.day14

import utils.BaseTest

class DayFourteenTest extends BaseTest:

  lazy val input: Seq[String] = Seq(
    "O....#....",
    "O.OO#....#",
    ".....##...",
    "OO.#O....O",
    ".O.....O#.",
    "O.#..O.#.#",
    "..O..#O..O",
    ".......O..",
    "#....###..",
    "#OO..#....",
  )

  "Day fourteen's solution" must:

    "calculate the correct load in part one" in:
      DayFourteen.partOne(input) mustBe 136

end DayFourteenTest
