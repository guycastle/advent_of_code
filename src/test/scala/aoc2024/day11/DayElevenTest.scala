package aoc2024.day11

import utils.BaseTest
import utils.syntax.*

class DayElevenTest extends BaseTest:

  final private val input: Seq[String] = "125 17".toSingleElementSeq

  private def calcPattern(i: Int): Unit =
//    val a = DayEleven.blink(Seq(i), 4).size
//    val b = DayEleven.blink(Seq(i), 5).size
    val c = DayEleven.blink(Seq(i), 75).size
//    val d = DayEleven.blink(Seq(i), 25).size
//    val d = DayEleven.blink(Seq(i + 1), 4).size
//    val e = DayEleven.blink(Seq(i + 1), 5).size
//    val f = DayEleven.blink(Seq(i + 1), 25).size
//    val g = DayEleven.blink(Seq(i + 2), 4).size
//    val h = DayEleven.blink(Seq(i + 2), 5).size
//    val j = DayEleven.blink(Seq(i + 2), 25).size
//    println("================")
//    println(s"$i (* 2024 = ${i * 2024}) blinked 4 = $a")
//    println(s"$i (* 2024 = ${i * 2024}) blinked 5 = $b")
    println(s"$i (* 2024 = ${i * 2024}) blinked 75 = $c")
//    println(s"$i (* 2024 = ${i * 2024}) blinked 25 = $d")
//    println(s"${i + 1} (* 2024 = ${(i + 1) * 2024}) blinked 4 = $d")
//    println(s"${i + 1} (* 2024 = ${(i + 1) * 2024}) blinked 5 = $e")
//    println(s"${i + 1} (* 2024 = ${(i + 1) * 2024}) blinked 6 = $f")
//    println(s"${i + 2} (* 2024 = ${(i + 2) * 2024}) blinked 4 = $g")
//    println(s"${i + 2} (* 2024 = ${(i + 2) * 2024}) blinked 5 = $h")
//    println(s"${i + 2} (* 2024 = ${(i + 2) * 2024}) blinked 6 = $j")
  end calcPattern

  "Processing the input for part one" must:

    "correctly calculate the number of stones after 25 blinks" in:
      DayEleven.partOne(input) mustBe 55312

  "Processing the input for part two" must:

    "correctly guestimate how many stones will be added after n blinks" in:
      (0 until 10).foreach(calcPattern)
      0 mustBe 0

    "correctly calculate the number of stones after 75 blinks" in:
      DayEleven.partTwo(input) mustBe 55312

end DayElevenTest
