package aoc2025.day10

import utils.DailyChallenge
import utils.custom.{Bit, Bits}

import java.time.LocalDate
import scala.annotation.tailrec

object DayTen extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 10)

  override def partOne(input: Seq[String]): Int = parseInput(input).map(amountOfButtonPresses).sum

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  private type JoltageRequirements = Seq[Int]

  // type Bit = 0 | 1; type Bits = Seq[Bit]
  private case class Machine(
      indicatorLights: Bits,
      buttonWiringSchematics: Seq[Bits],
      joltageRequirements: JoltageRequirements)

  private val amountOfButtonPresses: Machine => Int = machine =>
    findFewestAmountOfButtonPresses(target = machine.indicatorLights,
                                    options = machine.buttonWiringSchematics,
                                    currentNumberOfButtons = 1,
    )

  @tailrec
  private def findFewestAmountOfButtonPresses(
      target: Seq[Bit],
      options: Seq[Seq[Bit]],
      currentNumberOfButtons: Int): Int =
    if target.isEmpty || !target.contains(1) then 0
    else
      options
        .combinations(currentNumberOfButtons)
        .find: buttonCombo =>
          target
            .zip(buttonCombo.transpose)
            .forall:
              case (targetBit -> flips) => flips.sum % 2 == targetBit
      match
        case Some(combo) =>
          println(s"Found combo ${combo.map(_.mkString("(", ",", ")")).mkString(" & ")}")
          currentNumberOfButtons
        case None => findFewestAmountOfButtonPresses(target, options, currentNumberOfButtons + 1)
  end findFewestAmountOfButtonPresses

  private val parseInput: Seq[String] => Seq[Machine] = _.flatMap: line =>
    val components = line.split(" ").toSeq
    for
      indicatorLights <- components.collectFirst:
                           case s"[$indicatorLights]" => indicatorLights.collect[Bit]:
                               case '.' => 0
                               case '#' => 1
      joltage <- components.lastOption.collect:
                   case s"{$joltage}" => joltage.split(",").toSeq.flatMap(_.toIntOption)
      buttonWiring = components.collect:
                       case s"($buttonWiring)" =>
                         val wiring = buttonWiring.split(",").toSeq.flatMap(_.toIntOption)
                         indicatorLights.indices.map[Bit]:
                           case i if wiring.contains(i) => 1
                           case _                       => 0
    yield Machine(
      indicatorLights = indicatorLights,
      buttonWiringSchematics = buttonWiring,
      joltageRequirements = joltage,
    )
    end for
  end parseInput
end DayTen
