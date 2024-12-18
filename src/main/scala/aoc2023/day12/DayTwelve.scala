package aoc2023.day12

import utils.DailyChallenge

import java.time.LocalDate

object DayTwelve extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2023, 12, 12)

  override def partOne(input: Seq[String]): Long = 0

  override def partTwo(input: Seq[String]): Long = 0

  @main def run(): Unit = evaluate()

  enum Status:
    case Operational, Damaged, Unknown
  end Status

  case class Record(statuses: Seq[Status], contiguousDamaged: Seq[Int])

  lazy val parseInputLine: String => Record = line =>
    val (statuses, cont) = line.span(!_.isWhitespace)
    Record(
      statuses = statuses.map:
        case '?' => Status.Unknown
        case '#' => Status.Damaged
        case '.' => Status.Operational
      ,
      contiguousDamaged = cont.drop(1).withFilter(_ != ',').map(_.toString.toInt),
    )

  // TODO solve this
  def countArrangements(record: Record, count: Long = 0L): Long = ???

end DayTwelve
