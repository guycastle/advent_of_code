package aoc2024.day09

import utils.DailyChallenge
import utils.syntax.*

import java.time.LocalDate
import scala.annotation.tailrec

object DayNine extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 9)

  override def partOne(input: Seq[String]): Long =
    val defragmented = defragment(MemoryBlock.toFileSystemContent(parseInput(input)))
    checksum(defragmented)
  end partOne

  override def partTwo(input: Seq[String]): Long =
    val defragmented = defragmentBlocks(parseInput(input))
    checksum(MemoryBlock.toFileSystemContent(defragmented))
  end partTwo

  @main def run(): Unit = evaluate()

  private type FileSystemContent = Seq[Int]

  private type Drive = Seq[MemoryBlock]

  private def defragmentBlocks(drive: Drive): Drive =
    @tailrec
    def loop(idx: Int = drive.size - 1, defragmented: Drive = drive): Seq[MemoryBlock] = defragmented.lift(idx) match
      case Some(file: File) => defragmented.slice(0, idx).indexOfSlice((0 until file.length).map(_ => Free)) match
          case -1           => loop(idx = idx - 1, defragmented = defragmented)
          case freeSpaceIdx =>
            val temp    = defragmented.patch(idx, file.toFreeSpace, 1)
            val updated = temp.patch(freeSpaceIdx, file.toSingleElementSeq, file.length)
            loop(idx = idx - 1, defragmented = updated)
      case Some(Free) => loop(idx = idx - 1, defragmented = defragmented)
      case _          => defragmented
    end loop
    loop()
  end defragmentBlocks

  @tailrec
  private def defragment(fs: FileSystemContent): FileSystemContent = fs.indexOf(-1) match
    case -1            => fs
    case nextFreeSpace => fs.lastOption match
        case Some(-1) => defragment(fs.dropRight(1))
        case Some(id) => defragment(fs.updated(nextFreeSpace, id).dropRight(1))
        case None     => fs
  end defragment

  private val checksum: FileSystemContent => Long = _.zipWithIndex.foldLeft(0L):
    case (sum, (-1, _))   => sum
    case (sum, (id, idx)) => sum + (id * idx)
  end checksum

  sealed private trait MemoryBlock
  private case class File(id: Int, length: Int) extends MemoryBlock:
    lazy val toFreeSpace: Seq[MemoryBlock] = (0 until length).map(_ => Free)
  end File
  private case object Free extends MemoryBlock // is always 1 block
  private object MemoryBlock:
    val toFileSystemContent: Seq[MemoryBlock] => FileSystemContent = _.flatMap:
      case f: File => (0 until f.length).map(_ => f.id)
      case Free    => (-1).toSingleElementSeq
    end toFileSystemContent
  end MemoryBlock

  private val parseInput: Seq[String] => Drive = input =>
    input.filterNot(_.isBlank).headOption match
      case Some(line) => line.zipWithIndex.flatMap:
          case (d, i) if i % 2 == 0 => File(id = i / 2, length = d.asDigit).toSingleElementSeq
          case (d, _) => (0 until d.asDigit).map(_ => Free)
      case None => Seq.empty
  end parseInput

end DayNine
