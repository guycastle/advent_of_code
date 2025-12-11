package aoc2025.day09

import utils.DailyChallenge
import utils.syntax.*
import utils.custom.Point

import java.time.LocalDate

object DayNine extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 9)

  override def partOne(input: Seq[String]): Long = largestSquareSurfaceFromPoints(parseInput(input))

  override def partTwo(input: Seq[String]): Long = largestRectangleInPolygon(parseInput(input))

  @main def run(): Unit = evaluate()

  case class Rectangle(minX: Int, maxX: Int, minY: Int, maxY: Int):
    lazy val area: Long      = (maxX - minX + 1L) * (maxY - minY + 1L)
    lazy val xCenter: Double = (minX + maxX) / 2D
    lazy val yCenter: Double = (minY + maxY) / 2D
  end Rectangle
  private object Rectangle:
    def fromPoints(a: Point, b: Point): Rectangle =
      Rectangle(minX = a.x.min(b.x), maxX = a.x.max(b.x), minY = a.y.min(b.y), maxY = a.y.max(b.y))
  end Rectangle

  private val largestRectangleInPolygon: Seq[Point] => Long = points =>
    val edges = points.zip(points.tail :+ points.head)

    points
      .combinations(2)
      .map:
        case Seq(a, b) =>
          val r = Rectangle.fromPoints(a, b)

          val vCut: Boolean = edges.exists:
            case (e1, e2) => e1.x == e2.x && e1.x > r.minX && e1.x < r.maxX && (r.minY to r.maxY).overlaps(
                (e1.y.min(e2.y)) to (e1.y.max(e2.y)),
              )
          end vCut

          val hCut: Boolean = edges.exists:
            case (e1, e2) => e1.y == e2.y && e1.y > r.minY && e1.y < r.maxY && (r.minX to r.maxX).overlaps(
                (e1.x.min(e2.x)) to (e1.x.max(e2.x)),
              )
          end hCut

          val isInside: Boolean = edges
            .count:
              case (e1, e2) => (e1.y > r.yCenter) != (e2.y > r.yCenter) &&
                e1.x == e2.x &&
                (r.minX to r.maxX).overlaps((e1.x.min(e2.x)) to (e1.x.max(e2.x))) &&
                r.xCenter < e1.x
            .isEven
          end isInside

          if !vCut && !hCut && isInside then r.area else 0L
          end if
      .max
  end largestRectangleInPolygon

  private val largestSquareSurfaceFromPoints: Seq[Point] => Long = _.combinations(2)
    .collect:
      case Seq(a, b) => Rectangle.fromPoints(a, b)
    .maxByOption(_.area)
    .map(_.area)
    .getOrElse(0L)

  private val parseInput: Seq[String] => Seq[Point] = _.collect:
    case s"$x,$y" =>
      for
        x <- x.toIntOption
        y <- y.toIntOption
      yield Point(x = x, y = y)
  .flatten

end DayNine
