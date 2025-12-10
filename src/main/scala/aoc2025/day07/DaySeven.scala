package aoc2025.day07

import utils.DailyChallenge
import utils.Syntax.some
import utils.custom.Coordinate

import java.time.LocalDate
import scala.annotation.tailrec

object DaySeven extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 7)

  override def partOne(input: Seq[String]): Long = startingPoint(input.toList)
    .map(start => splitBeams(Set(start.index), 1, start.manifold).size)
    .getOrElse(0)

  override def partTwo(input: Seq[String]): Long = startingPoint(input.toList)
    .map(start => acrossTheMultiverse(Coordinate(start.index, 1), start.manifold).branchResult)
    .getOrElse(0)

  @main def run(): Unit = evaluate()

  private type StartingPoint  = (index: Int, manifold: List[String])
  private type ResultAndCache = (branchResult: Long, cache: Map[Coordinate, Long])
  private type Splitter       = '^'
  private type Empty          = '.'

  private val startingPoint: List[String] => Option[StartingPoint] =
    case head :: tail if head.contains('S') => (index = head.indexOf('S'), manifold = tail).some
    case _                                  => None
  end startingPoint

  @tailrec
  private def splitBeams(
      indices: Set[Int],
      currentRow: Int,
      manifold: List[String],
      splitterCoordinates: Set[Coordinate] = Set.empty): Set[Coordinate] = manifold match
    case Nil          => splitterCoordinates
    case head :: tail =>
      val (updatedSplitterCoordinates, updatedIndices) = indices.foldLeft((splitterCoordinates, Set.empty[Int])):
        case ((splitters, newIndices), xIdx) => head.lift(xIdx) match
            case Some(_: Splitter) => (splitters + Coordinate(xIdx, currentRow), newIndices ++ Set(xIdx - 1, xIdx + 1))
            case Some(_: Empty)    => (splitters, newIndices + xIdx)
            case _                 => (splitters, newIndices)
      splitBeams(updatedIndices, currentRow + 1, tail, updatedSplitterCoordinates)

  private def acrossTheMultiverse(
      position: Coordinate,
      manifold: List[String],
      cache: Map[Coordinate, Long] = Map.empty): ResultAndCache = cache.get(position) match
    case Some(result) => (result, cache)
    case None         => manifold match
        case head :: tail => head.lift(position.x) match
            case Some(_: Splitter) =>
              val (leftResult, updatedLeftCache) =
                acrossTheMultiverse(position.copy(x = position.x - 1, position.y + 1), tail, cache)
              val (rightResult, updatedCache) =
                acrossTheMultiverse(position.copy(x = position.x + 1, position.y + 1), tail, updatedLeftCache)
              (leftResult + rightResult, updatedCache)
            case Some(_) =>
              val (result, updatedCache) = acrossTheMultiverse(position.copy(y = position.y + 1), tail, cache)
              (result, updatedCache + (position -> result))
            case None => (0, cache)
        case Nil => (1, cache)
  end acrossTheMultiverse
end DaySeven
