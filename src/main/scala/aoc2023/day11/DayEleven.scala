package aoc2023.day11

import utils.DailyChallenge

import java.time.LocalDate
import scala.annotation.tailrec
import scala.math.abs

object DayEleven extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2023, 12, 11)

  override def partOne(input: Seq[String]): Long =
    parseInput(input = input, expandBy = 1).map(distanceBetweenGalaxies).sum

  override def partTwo(input: Seq[String]): Long =
    parseInput(input = input, expandBy = 999999).map(distanceBetweenGalaxies).sum

  @main def run(): Unit = evaluate()

  case class Galaxy(x: Long, y: Long)

  private type GalaxyPair = (Galaxy, Galaxy)

  lazy val distanceBetweenGalaxies: GalaxyPair => Long =
    case (Galaxy(x1, y1), Galaxy(x2, y2)) => abs(x1 - x2) + abs(y1 - y2)

  def parseInput(input: Seq[String], expandBy: Int): Seq[GalaxyPair] =
    val (universe, _) = input.headOption match
      case Some(row) =>
        val verticalEmptySpaceIndices = row.indices.filter(i => !input.exists(_.lift(i).contains('#')))
        input.zipWithIndex.foldLeft((Seq.empty[Galaxy], 0L)):
          case ((galaxies, yOffset), (row, y)) =>
            val newYOffset = if row.forall(_ == '.') then yOffset + expandBy else yOffset
            val (rowGalaxies, _) = row.zipWithIndex.foldLeft((Seq.empty[Galaxy], 0L)):
              case ((rowGalaxies, xOffset), (space, x)) =>
                if verticalEmptySpaceIndices contains x then (rowGalaxies, xOffset + expandBy)
                else
                  space match
                    case '#' => (rowGalaxies :+ Galaxy(x = x + xOffset, y = y + newYOffset), xOffset)
                    case _   => (rowGalaxies, xOffset)
            (galaxies ++ rowGalaxies, newYOffset)
      case None => throw new IllegalArgumentException("Empty universe")

    @tailrec
    def pair(galaxies: Seq[Galaxy], pairs: Seq[GalaxyPair] = Seq.empty): Seq[GalaxyPair] = galaxies.headOption match
      case Some(galaxy) if galaxies.size >= 2 =>
        val newPairs = galaxies.drop(1).map(other => (galaxy, other))
        pair(galaxies = galaxies.drop(1), pairs = pairs ++ newPairs)
      case _ => pairs

    pair(universe)

end DayEleven
