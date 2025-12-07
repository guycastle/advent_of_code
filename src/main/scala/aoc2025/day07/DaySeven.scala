package aoc2025.day07

import utils.DailyChallenge
import utils.structs.Coordinate

import java.time.LocalDate
import scala.annotation.tailrec

object DaySeven extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 7)

  override def partOne(input: Seq[String]): Int = beamSplitter(input.toList)

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  private def beamSplitter(input: List[String]): Int = input match
    case head :: tail if head.contains('S') => splitBeams(Set(head.indexOf('S')), 1, tail).splitterCoordinates.size
    case _                                  =>
      println("No starting beam found")
      0
  end beamSplitter

  private type Splitter        = '^'
  private type Empty           = '.'
  private type SplitBeamResult = (splitterCoordinates: Set[Coordinate], beamCoordinates: Set[Coordinate])

  @tailrec
  private def splitBeams(
      indices: Set[Int],
      currentRow: Int,
      manifold: List[String],
      splitterCoordinates: Set[Coordinate] = Set.empty,
      beamCoordinates: Set[Coordinate] = Set.empty): SplitBeamResult = manifold match
    case Nil          => (splitterCoordinates = splitterCoordinates, beamCoordinates = beamCoordinates)
    case head :: tail =>
      val (updatedBeamCoordinate, updatedSplitterCoordinates, updatedIndices) = indices.foldLeft((beamCoordinates, splitterCoordinates, Set.empty[Int])):
        case ((beams, splitters, newIndices), xIdx) => head.lift(xIdx) match
            case Some(_: Splitter) =>
              val newBeamIndices = Set(xIdx - 1, xIdx + 1)
              (beams ++ newBeamIndices.map(x => Coordinate(x = x, y = currentRow)), splitters + Coordinate(xIdx, currentRow), newIndices ++ newBeamIndices)
            case Some(_: Empty) => (beams + Coordinate(xIdx, currentRow), splitters, newIndices + xIdx)
            case _              => (beams, splitters, newIndices)
      splitBeams(updatedIndices, currentRow + 1, tail, updatedSplitterCoordinates, updatedBeamCoordinate)

end DaySeven
