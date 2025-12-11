package aoc2025.day11

import utils.BaseTest

class DayElevenTest extends BaseTest:

  private val inputPart1: Seq[String] = """aaa: you hhh
                                     |you: bbb ccc
                                     |bbb: ddd eee
                                     |ccc: ddd eee fff
                                     |ddd: ggg
                                     |eee: out
                                     |fff: out
                                     |ggg: out
                                     |hhh: ccc fff iii
                                     |iii: out""".stripMargin.split("\n").toIndexedSeq

  private val inputPart2: Seq[String] = """svr: aaa bbb
                                          |aaa: fft
                                          |fft: ccc
                                          |bbb: tty
                                          |tty: ccc
                                          |ccc: ddd eee
                                          |ddd: hub
                                          |hub: fff
                                          |eee: dac
                                          |dac: fff
                                          |fff: ggg hhh
                                          |ggg: out
                                          |hhh: out""".stripMargin.split("\n").toIndexedSeq

  "Parsed input in part one" must:

    "calculate the correct output" in:
      DayEleven.partOne(inputPart1) mustBe 5

  "Parsed input in part two" must:

    "calculate the correct output" in:
      DayEleven.partTwo(inputPart2) mustBe 2

end DayElevenTest
