package aoc2024.day09

import utils.DailyChallenge

import java.time.LocalDate
import scala.annotation.tailrec

object DayNine extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 9)

  override def partOne(input: Seq[String]): Long =
    val defragmented = defragment(parseInput(input))
    checksum(defragmented)

  override def partTwo(input: Seq[String]): Long = 0

  @main def run(): Unit = evaluate()

  private type MemoryBlocks = Seq[Long]

  @tailrec
  private def defragment(memory: MemoryBlocks): MemoryBlocks =
    memory.indexOf(-1) match
      case -1 => memory
      case nextFreeSpace =>
        memory.lastOption match
          case Some(-1) => defragment(memory.dropRight(1))
          case Some(id) => defragment(memory.patch(nextFreeSpace, Seq(id), 1).dropRight(1))
          case None     => memory
  end defragment

  private val checksum: MemoryBlocks => Long = _.zipWithIndex.foldLeft(0L):
    case (sum, (id, idx)) => sum + (id * idx)
  end checksum

  private val parseInput: Seq[String] => MemoryBlocks = input =>
    input.filterNot(_.isBlank).headOption match
      case Some(line) =>
        line.zipWithIndex.flatMap:
          case (d, i) if i % 2 == 0 => (0 until d.asDigit).map(_ => i / 2)
          case (d, _) => (0 until d.asDigit).map(_ => -1)
      case None => Seq.empty
  end parseInput

end DayNine
