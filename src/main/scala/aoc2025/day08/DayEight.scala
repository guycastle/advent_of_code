package aoc2025.day08

import utils.DailyChallenge
import utils.syntax.*

import java.time.LocalDate
import scala.annotation.tailrec

object DayEight extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 8)

  override def partOne(input: Seq[String]): Int = evaluatePartOne(input)

  override def partTwo(input: Seq[String]): Int = createCircuitsUntilSingleCircuit(parseInput(input))
    .map(pair => pair.a.x * pair.b.x)
    .getOrElse(0)

  @main def run(): Unit = evaluate()

  def evaluatePartOne(input: Seq[String], connections: Int = 1000): Int =
    createCircuits(parseInput(input), connections).map(_.size).sorted.takeRight(3).product
  end evaluatePartOne

  case class Position(x: Int, y: Int, z: Int):
    def distanceBetween(other: Position): Double =
      math.sqrt(math.pow(this.x - other.x, 2) + math.pow(this.y - other.y, 2) + math.pow(this.z - other.z, 2))
  end Position

  private type Circuit = Set[Position]
  extension (circuit: Circuit) private def isConnected(pos: Position*): Boolean = pos.exists(circuit.contains)
  end extension

  private case class BoxPairDistance(a: Position, b: Position, distance: Double):
    def toCircuit: Circuit = Set(a, b)
  end BoxPairDistance

  private def createCircuits(boxes: Seq[Position], amountOfConnections: Int): Seq[Circuit] =
    toBoxPairDistanceAscDistance(boxes)
      .take(amountOfConnections)
      .foldLeft(Seq.empty[Circuit])((circuits, boxPair) => connectBoxPairToCircuit(boxPair, circuits))
  end createCircuits

  private val toBoxPairDistanceAscDistance: Seq[Position] => Seq[BoxPairDistance] = _.combinations(2)
    .collect:
      case Seq(a, b) => BoxPairDistance(a = a, b = b, distance = a.distanceBetween(b))
    .toSeq
    .sortBy(_.distance)
  end toBoxPairDistanceAscDistance

  private def createCircuitsUntilSingleCircuit(boxes: Seq[Position]): Option[BoxPairDistance] =
    @tailrec
    def connectAll(boxPairs: List[BoxPairDistance], circuits: Seq[Circuit] = Seq.empty): Option[BoxPairDistance] =
      boxPairs match
        case pair :: remaining => connectBoxPairToCircuit(pair, circuits) match
            case Seq(circuit) if circuit.size == boxes.size => pair.some
            case ongoing                                    => connectAll(remaining, ongoing)
        case Nil => None
    end connectAll
    connectAll(toBoxPairDistanceAscDistance(boxes).toList)
  end createCircuitsUntilSingleCircuit

  private def connectBoxPairToCircuit(boxPair: BoxPairDistance, circuits: Seq[Circuit]): Seq[Circuit] =
    (circuits.indexWhere(_.isConnected(boxPair.a)), circuits.indexWhere(_.isConnected(boxPair.b))) match
      // Adding new circuit...
      case (-1, -1) => circuits :+ boxPair.toCircuit
      // Both boxes are already connected to different circuits, merging...
      case (idxA, idxB) if idxA >= 0 && idxB >= 0 && idxA != idxB =>
        circuits.updated(idxA, circuits(idxA) ++ circuits(idxB)).patch(idxB, Nil, 1)
      // Both boxes are already connected to the same circuit, skipping...
      case (idxA, idxB) if idxA >= 0 && idxB >= 0 => circuits
      // Adding new box to existing circuit...
      case (idxA, idxB) =>
        val existingIdx = math.max(idxA, idxB)
        circuits.updated(existingIdx, circuits(existingIdx) ++ boxPair.toCircuit)
  end connectBoxPairToCircuit

  private val parseInput: Seq[String] => Seq[Position] = _.collect:
    case s"$x,$y,$z" => Seq(x, y, z).flatMap(_.toIntOption) match
        case Seq(x, y, z) => Position(x = x, y = y, z = z).some
  .flatten

end DayEight
