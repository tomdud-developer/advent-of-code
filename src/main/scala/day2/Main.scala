package day2

import scala.Option.option2Iterable
import scala.util.matching.Regex

object Main extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day2/input1.txt")
  val green = 13
  val red = 12
  val blue = 14

    // I think it is a good task for regexes
  val gamePattern: Regex = "Game (\\d+)".r
  val greenPattern: Regex = "(\\d+) (green)".r
  val bluePattern: Regex = "(\\d+) (blue)".r
  val redPattern: Regex = "(\\d+) (red)".r

  case class Game (
    gameNumber: Int,
    sets: Array[String]
  )

  val result = file.getLines().map(
    line => {
      val gameNumber = gamePattern.findFirstIn(line).get
      Game(
        "\\d+".r.findFirstIn(gameNumber).get.toInt,
        line.substring(gameNumber.length + 2)
          .split(';')
      )
    }
  ).filter(
    game => {
      game.sets.count(set => {
        def getNumberOfColorCubeInSet(regex: Regex, set: String): Int = {
          regex.findFirstMatchIn(set).collect(_.group(1)).map(_.toInt).getOrElse(0)
        }

        val greenCubesInSet = getNumberOfColorCubeInSet(greenPattern, set)
        val blueCubesInSet = getNumberOfColorCubeInSet(bluePattern, set)
        val redCubesInSet = getNumberOfColorCubeInSet(redPattern, set)

        greenCubesInSet <= green && redCubesInSet <= red && blueCubesInSet <= blue
      }) == game.sets.length
    }
  ).map(_.gameNumber).sum

  println(result)
}