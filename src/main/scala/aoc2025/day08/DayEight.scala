package aoc2025.day08

import utils.DailyChallenge
import utils.Syntax.*

import java.time.LocalDate

object DayEight extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 8)

  override def partOne(input: Seq[String]): Int = evaluatePartOne(input)

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  def evaluatePartOne(input: Seq[String], connections: Int = 1000): Int =
    createCircuits(parseInput(input), connections).map(_.size).sorted.takeRight(3).product
  end evaluatePartOne

  case class Position(x: Int, y: Int, z: Int):
    def distanceBetween(other: Position): Double = math.sqrt(math.pow(this.x - other.x, 2) + math.pow(this.y - other.y, 2) + math.pow(this.z - other.z, 2))
  end Position

  private type Circuit = Set[Position]
  extension (circuit: Circuit)
    private def isConnected(pos: Position*): Boolean = pos.exists(circuit.contains)
  end extension

  private case class JunctionBoxesDistance(a: Position, b: Position, distance: Double):
    def toCircuit: Circuit = Set(a, b)
  end JunctionBoxesDistance
  
  private def createCircuits(boxes: Seq[Position], amountOfConnections: Int): Seq[Circuit] =
    boxes.combinations(2).collect:
      case Seq(a, b) => JunctionBoxesDistance(a = a, b = b, distance = a.distanceBetween(b))
    .toSeq
    .sortBy(_.distance)
    .take(amountOfConnections)
    .foldLeft(Seq.empty[Circuit]):
      case (circuits, boxPair) =>
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
  end createCircuits

  private val parseInput: Seq[String] => Seq[Position] = _.collect:
    case s"$x,$y,$z" => Seq(x, y, z).flatMap(_.toIntOption) match
      case Seq(x, y, z) => Position(x = x, y = y, z = z).some
  .flatten

end DayEight
