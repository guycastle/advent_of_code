package utils

object custom:

  case class Coordinate(x: Int, y: Int)
  type Point = Coordinate
  object Point:
    def apply(x: Int, y: Int): Point = Coordinate(x, y)
  end Point

  type Bit  = 0 | 1
  type Bits = Seq[Bit]
  extension (bits: Bits) def toMask: Int = bits.foldLeft(0)((acc, bit) => (acc << 1) | bit)
  end extension
end custom
