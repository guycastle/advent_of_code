package aoc2024.day08

import utils.DailyChallenge
import utils.Syntax.*

import java.time.LocalDate
import scala.annotation.tailrec

object DayEight extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 8)

  override def partOne(input: Seq[String]): Int =
    val (width, height) = getWidthAndHeight(input)
    val antennaGroups   = parseInput(input)
    findAllUniqueAntinodes(
      antennaGroups = antennaGroups,
      width = width,
      height = height,
      maxAntinodes = 1.some,
    ).size
  end partOne

  override def partTwo(input: Seq[String]): Int =
    val (width, height) = getWidthAndHeight(input)
    val antennaGroups   = parseInput(input)
    val antinodes       = findAllUniqueAntinodes(
      antennaGroups = antennaGroups,
      width = width,
      height = height,
    ) ++ antennaGroups.values.flatten.toSet // add the antenna's themselves to the unique set of positions
    antinodes.size
  end partTwo

  @main def run(): Unit = evaluate()

  private type Position = (Int, Int)

  private type AntennaGroups = Map[Char, Set[Position]]

  private type Width  = Int
  private type Height = Int

  private def findAllUniqueAntinodes(
      antennaGroups: Map[Char, Set[Position]],
      width: Width,
      height: Height,
      maxAntinodes: Option[Int] = None): Set[Position] =

    @tailrec
    def calculateAntinode(
        position: Position,
        move: Position => Position,
        antinodes: Set[Position] = Set.empty): Set[Position] = maxAntinodes match
      case Some(value) if antinodes.size >= value => antinodes
      case _                                      => move(position) match
          case newPosition @ (x, y) if (0 until width).contains(x) && (0 until height).contains(y) =>
            calculateAntinode(newPosition, move, antinodes + newPosition)
          case other => antinodes

    antennaGroups.values.foldLeft(Set.empty): (allAntinodes, antennasInGroup) =>
      antennasInGroup.toSeq
        .combinations(2)
        .foldLeft(allAntinodes):
          case (antinodes, Seq((x1, y1), (x2, y2))) =>
            val (dx, dy) = (x1 - x2, y1 - y2)
            antinodes ++
              calculateAntinode((x1, y1), (x, y) => (x + dx, y + dy)) ++
              calculateAntinode((x2, y2), (x, y) => (x - dx, y - dy))
          case (antinodes, _) => antinodes
  end findAllUniqueAntinodes

  private val parseInput: Seq[String] => AntennaGroups = input =>
    input.zipWithIndex.foldLeft(Map.empty[Char, Set[Position]]):
      case (map, (row, y)) => row.zipWithIndex.foldLeft(map):
          case (rowMap, (column, x)) if column != '.' =>
            rowMap.updatedWith(column)(_.orElse(Set.empty[Position].some).map(_ + (x -> y)))
          case (rowMap, _) => rowMap
  end parseInput

  private val getWidthAndHeight: Seq[String] => (Width, Height) =
    input => (input.headOption.map(_.length).getOrElse(0), input.size)

end DayEight
