package aoc2025.day11

import utils.DailyChallenge

import java.time.LocalDate

object DayEleven extends DailyChallenge[Int]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 11)

  override def partOne(input: Seq[String]): Int =
    val devices     = parseInput(input)
    val (result, _) = cachedPathCount(deviceId = in, devices = devices)
    result
  end partOne

  override def partTwo(input: Seq[String]): Int = 0

  @main def run(): Unit = evaluate()

  opaque type DeviceId = String

  type ResultAndCache = (result: Int, cache: Map[DeviceId, Int])

  private val in: DeviceId  = "you"
  private val out: DeviceId = "out"

  private def cachedPathCount(
      deviceId: DeviceId,
      devices: Map[DeviceId, Set[DeviceId]],
      cache: Map[DeviceId, Int] = Map.empty): ResultAndCache =
    lazy val deviceOutputs = devices.getOrElse(deviceId, Set.empty)
    cache.get(deviceId) match
      case Some(count)                         => (count, cache)
      case None if deviceOutputs.isEmpty       => (0, cache.updated(deviceId, 0))
      case None if deviceOutputs.contains(out) => (1, cache.updated(deviceId, 1))
      case None                                => deviceOutputs.foldLeft((0, cache)):
          case ((accResult, accCache), outputDeviceId) =>
            val (res, updatedCache) = cachedPathCount(outputDeviceId, devices, accCache)
            (accResult + res, updatedCache)
    end match
  end cachedPathCount

  private val parseInput: Seq[String] => Map[DeviceId, Set[DeviceId]] = _.collect:
    case s"$id: $outputs" => id -> outputs.split("\\s").toSet
  .toMap
end DayEleven
