package aoc2024.day10

import utils.DailyChallenge

import java.time.LocalDate

object DayTen extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2024, 12, 10)

  override def partOne(input: Seq[String]): Int =
    val (map, trailHeads) = parseInput(input)
    trailHeads.map(th => trailScoreBySummits(position = th, map = map).size).sum
  end partOne

  override def partTwo(input: Seq[String]): Int =
    val (map, trailHeads) = parseInput(input)
    trailHeads.map(th => trailScoreByPathCount(position = th, map = map)).sum
  end partTwo

  @main def run(): Unit = evaluate()

  private type TopographicalMap = Seq[Seq[Int]]
  extension (map: TopographicalMap)
    private def heightAt(position: Position): Option[Int] =
      val x -> y = position
      map.lift(y).flatMap(_.lift(x))
  end extension
  private object TopographicalMap:
    val empty: TopographicalMap = Seq.empty
  end TopographicalMap

  private type Position = (Int, Int)

  enum Direction(val move: Position => Position):
    case Up extends Direction(move = (x, y) => (x, y - 1))
    case Down extends Direction(move = (x, y) => (x, y + 1))
    case Left extends Direction(move = (x, y) => (x - 1, y))
    case Right extends Direction(move = (x, y) => (x + 1, y))

  end Direction
  object Direction:
    val skipOrigin: Direction => Seq[Direction] =
      case Up    => Seq(Left, Right, Up)
      case Down  => Seq(Left, Right, Down)
      case Left  => Seq(Up, Down, Left)
      case Right => Seq(Up, Down, Right)
    end skipOrigin
  end Direction

  private def newPositionsAndPotentialDirections(
      position: Position,
      currentHeight: Int,
      directions: Seq[Direction],
      map: TopographicalMap,
  ): Seq[(Position, Seq[Direction])] = directions
    .foldLeft(Seq.empty[(Position, Seq[Direction])]):
      case (positionAndDirections, direction) =>
        val newPosition = direction.move(position)
        map.heightAt(newPosition) match
          case Some(height) if height == currentHeight + 1 =>
            positionAndDirections :+ (newPosition -> Direction.skipOrigin(direction))
          case _ => positionAndDirections
        end match
  end newPositionsAndPotentialDirections

  private def trailScoreBySummits(
      position: Position,
      map: TopographicalMap,
      currentHeight: Int = 0,
      summits: Set[Position] = Set.empty,
      directions: Seq[Direction] = Direction.values.toSeq,
  ): Set[Position] =
    if currentHeight == 9 then summits + position
    else
      newPositionsAndPotentialDirections(position = position,
                                         currentHeight = currentHeight,
                                         directions = directions,
                                         map = map,
      ) match
        case Nil                                     => summits
        case (newPosition, directionsToCheck) :: Nil => trailScoreBySummits(position = newPosition,
                                                                            currentHeight = currentHeight + 1,
                                                                            summits = summits,
                                                                            map = map,
                                                                            directions = directionsToCheck,
          )
        case newPositions => newPositions.foldLeft(summits):
            case (summits, (newPosition, directionsToCheck)) => trailScoreBySummits(position = newPosition,
                                                                                    currentHeight = currentHeight + 1,
                                                                                    summits = summits,
                                                                                    map = map,
                                                                                    directions = directionsToCheck,
              )
  end trailScoreBySummits

  private def trailScoreByPathCount(
      position: Position,
      map: TopographicalMap,
      currentHeight: Int = 0,
      count: Int = 0,
      directions: Seq[Direction] = Direction.values.toSeq,
  ): Int =
    if currentHeight == 9 then count + 1
    else
      newPositionsAndPotentialDirections(position = position,
                                         currentHeight = currentHeight,
                                         directions = directions,
                                         map = map,
      ) match
        case Nil                                     => count
        case (newPosition, directionsToCheck) :: Nil => trailScoreByPathCount(position = newPosition,
                                                                              currentHeight = currentHeight + 1,
                                                                              count = count,
                                                                              map = map,
                                                                              directions = directionsToCheck,
          )
        case newPositions => newPositions.foldLeft(count):
            case (count, (newPosition, directionsToCheck)) => trailScoreByPathCount(position = newPosition,
                                                                                    currentHeight = currentHeight + 1,
                                                                                    count = count,
                                                                                    map = map,
                                                                                    directions = directionsToCheck,
              )
  end trailScoreByPathCount

  private val parseInput: Seq[String] => (TopographicalMap, Seq[Position]) =
    _.zipWithIndex.foldLeft((TopographicalMap.empty, Seq.empty[Position])):
      case ((map, heads), (row, y)) =>
        val (parsedRow, trailHeads) = row.zipWithIndex.foldLeft((Seq.empty[Int], heads)):
          case ((map, heads), (c, x)) =>
            val height = c.asDigit
            (map :+ height, if height == 0 then heads :+ (x, y) else heads)
        (map :+ parsedRow, trailHeads)
  end parseInput

end DayTen
