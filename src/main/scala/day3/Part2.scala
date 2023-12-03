package day3

import scala.util.matching.Regex

object Part2 extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day3/input1.txt")
  val lines = file.getLines().toList


  case class PartCandidate (
                             number: String,
                             lineNumber: Int,
                             startIndex: Int,
                             pointsToCheck: List[(Int, Int)])

  def generatePointsToCheck(row: Int, startIndex: Int, length: Int, maxIndex: Int): List[(Int, Int)] = {
    var pointList: List[(Int,Int)]  = (startIndex - 1 to startIndex + length)
      .flatMap(c => Seq((row - 1, c), (row + 1, c))).toList

    pointList :+= (row, startIndex - 1)
    pointList :+= (row, startIndex + length)

    pointList.filter(
      t => t._1 >= 0 && t._1 < lines.length && t._2 >= 0 && t._2 <= maxIndex
    )
  }

  val partCandidates = lines.zipWithIndex.flatMap { case (line, row) =>
    "\\d+".r.findAllMatchIn(line).map { m =>
      PartCandidate(
        m.matched,
        row,
        m.start,
        generatePointsToCheck(row, m.start, m.matched.length, line.length - 1)
      )
    }
  }

  case class GearCandidate (
                             number: Int,
                             point: (Int, Int)
                           )

  val arr: Array[Array[Char]] = lines.map(_.split("").map(_.charAt(0))).toArray
  val gearsCandidates = partCandidates.map(
    candidate => GearCandidate(
      candidate.number.toInt,
      candidate.pointsToCheck.find(t => arr(t._1)(t._2).equals('*')).getOrElse((-1, -1))
    )
  ).filter(_.point != (-1, -1))

  val result = gearsCandidates
    .groupBy(_.point)
    .filter(enter => enter._2.length  == 2)
    .map(enter => enter._2.head.number * enter._2(1).number)
    .sum

  println(result)
}