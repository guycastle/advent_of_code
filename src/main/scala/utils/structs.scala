package utils

object structs:

  case class Coordinate(x: Int, y: Int)
  type Point = Coordinate
  object Point:
    def apply(x: Int, y: Int): Point = Coordinate(x, y)
  end Point
end structs
