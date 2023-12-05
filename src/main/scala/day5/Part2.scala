package day5

import scala.collection.mutable

object Part2 extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day5/input.txt")
  val lines = file.getLines().toList
  val linesBuffer = lines.toBuffer

  val seeds = getSeedsFromBuffer()
  val map = getMapFromBuffer().toMap

  val location = seeds.zipWithIndex.map { case (seed, index) =>
    println(index)
    convertSeedToLocationBasedOnMap(seed)
  }.min

  print(location)

  private def getSeedsFromBuffer(): Iterator[Long] = {
    linesBuffer.remove(0).split(":")(1).trim.split(" ")
      .map(_.toLong)
      .sliding(2, 2).flatMap(arr => arr(0) to arr(1))
  }

  private def getMapFromBuffer(): mutable.Map[Long, List[Converter]]  = {
    val numberPattern = """(\d+)\s+(\d+)\s+(\d+)""".r
    val mapNamePattern = """([\w-]+)\s+map\W*""".r

    val map = mutable.Map[Long, List[Converter]]()
    var currentMapper: Long = 0

    linesBuffer.foreach {
      case numberPattern(num1, num2, num3) =>
        map(currentMapper) :+= Converter(num1.toLong, num2.toLong, num3.toLong)
      case mapNamePattern(mapName) =>
        currentMapper += 1
        map += currentMapper -> List.empty[Converter]
      case _ => // Do nothing
    }

    map
  }

  private def convertSeedToLocationBasedOnMap(seed: Long): Long = {
    (1L to map.size.toLong).fold(seed)(
      (valueToConvert, mapperIndex) => {
        //println(s"Converting using mapper ${mapperIndex} for $valueToConvert")
        convertByConverters(valueToConvert, map(mapperIndex))
      }
    )
  }

  private def convertByConverters(valueToConvert: Long, converters: List[Converter]): Long = {
    //println(s"Using converters $converters")
    converters.find(
        c => c.sourceRangeStart to c.sourceRangeStart + c.range contains valueToConvert
      ).map(c => {
        //println(s"Founded converter $c and using it on value $valueToConvert")
        valueToConvert + (c.destinationRangeStart - c.sourceRangeStart)
      })
      .getOrElse(valueToConvert)
  }

  case class Converter(
                        destinationRangeStart: Long,
                        sourceRangeStart: Long,
                        range: Long
                      )
}




