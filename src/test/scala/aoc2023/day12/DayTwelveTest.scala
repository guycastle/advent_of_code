package aoc2023.day12

import aoc2023.day12.DayTwelve.Record
import aoc2023.day12.DayTwelve.Status._
import utils.BaseTest

class DayTwelveTest extends BaseTest:

  lazy val test: Seq[String] = Seq(
    "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1",
  )

  "Day twelve's solution" must:

    "parse the input correctly" in:
      test.headOption.map(DayTwelve.parseInputLine).value mustBe Record(
        statuses = Seq(Unknown, Unknown, Unknown, Operational, Damaged, Damaged, Damaged),
        contiguousDamaged = Seq(1, 1, 3),
      )

    "calculate the correct sum of possible arrangements" ignore:
      DayTwelve.partOne(test) mustBe 2

end DayTwelveTest
