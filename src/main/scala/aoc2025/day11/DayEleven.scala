package aoc2025.day11

import utils.DailyChallenge

import java.time.LocalDate
object DayEleven extends DailyChallenge[Long]:

  override lazy val day: LocalDate = LocalDate.of(2025, 12, 11)

  override def partOne(input: Seq[String]): Long =
    val devices     = parseInput(input)
    val (result, _) = cachedPathCount(deviceId = inPartOne, devices = devices)
    result
  end partOne

  override def partTwo(input: Seq[String]): Long =
    val devices     = parseInput(input)
    val (result, _) = cachedPaths(deviceId = inPartTwo, devices = devices)
    result
  end partTwo

  @main def run(): Unit = evaluate()

  private type DeviceId = String

  private type ResultAndDeviceIdCache = (result: Long, cache: Map[DeviceId, Long])

  private val inPartOne: DeviceId            = "you"
  private val inPartTwo: DeviceId            = "svr"
  private val requiredDevices: Set[DeviceId] = Set("dac", "fft")
  private val out: DeviceId                  = "out"

  private def cachedPathCount(
      deviceId: DeviceId,
      devices: Map[DeviceId, Set[DeviceId]],
      cache: Map[DeviceId, Long] = Map.empty): ResultAndDeviceIdCache =
    lazy val deviceOutputs = devices.getOrElse(deviceId, Set.empty)
    cache.get(deviceId) match
      case Some(count)                         => (count, cache)
      case None if deviceOutputs.isEmpty       => (0, cache.updated(deviceId, 0))
      case None if deviceOutputs.contains(out) => (1, cache.updated(deviceId, 1))
      case None                                => deviceOutputs.foldLeft((0L, cache)):
          case ((accResult, accCache), outputDeviceId) =>
            val (res, updatedCache) = cachedPathCount(outputDeviceId, devices, accCache)
            (accResult + res, updatedCache)
    end match
  end cachedPathCount

  private type ContextCacheKey       = (DeviceId, Set[DeviceId])
  private type ResultAndContextCache = (result: Long, cache: Map[ContextCacheKey, Long])

  private def cachedPaths(
      deviceId: DeviceId,
      collected: Set[DeviceId] = Set.empty,
      devices: Map[DeviceId, Set[DeviceId]],
      cache: Map[ContextCacheKey, Long] = Map.empty): ResultAndContextCache =
    val currentCollected = if requiredDevices.contains(deviceId) then collected + deviceId else collected
    val cacheKey         = (deviceId, currentCollected)

    cache.get(cacheKey) match
      case Some(count) => (count, cache)
      case None        => devices.get(deviceId) match
          case Some(deviceOutputs) if deviceOutputs.contains(out) =>
            val result = if requiredDevices.subsetOf(currentCollected) then 1 else 0
            (result, cache.updated(cacheKey, result))
          case Some(deviceOutputs) if deviceOutputs.nonEmpty =>
            val (finalResult, finalCache) = deviceOutputs.foldLeft((0L, cache)):
              case ((accResult, accCache), outputDeviceId) =>
                val (res, updatedCache) = cachedPaths(outputDeviceId, currentCollected, devices, accCache)
                (accResult + res, updatedCache)
            (finalResult, finalCache.updated(cacheKey, finalResult))
          case _ => (0, cache.updated(cacheKey, 0))
    end match
  end cachedPaths

  private val parseInput: Seq[String] => Map[DeviceId, Set[DeviceId]] = _.collect:
    case s"$id: $outputs" => id -> outputs.split("\\s").toSet
  .toMap
end DayEleven
