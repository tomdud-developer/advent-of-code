package day2

import scala.util.matching.Regex

object Part2 extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day2/input1.txt")

  val gamePattern: Regex = "Game (\\d+)".r
  val greenPattern: Regex = "(\\d+) (green)".r
  val bluePattern: Regex = "(\\d+) (blue)".r
  val redPattern: Regex = "(\\d+) (red)".r

  case class Game (
    gameNumber: Int,
    sets: String
  )

  val result = file.getLines().map(
    line => {
      val gameNumber = gamePattern.findFirstIn(line).get
      Game(
        "\\d+".r.findFirstIn(gameNumber).get.toInt,
        line.substring(gameNumber.length + 2)
      )
    }
  ).map(
    game => {
        def getMaxNumberOfColorCubeInSets(regex: Regex, sets: String): Int =
          regex.findAllMatchIn(sets).collect(_.group(1)).map(_.toInt).maxOption.getOrElse(0)

        val maxGreenCubesInSet = getMaxNumberOfColorCubeInSets(greenPattern, game.sets)
        val maxBlueCubesInSet = getMaxNumberOfColorCubeInSets(bluePattern, game.sets)
        val maxRedCubesInSet = getMaxNumberOfColorCubeInSets(redPattern, game.sets)

        maxGreenCubesInSet * maxBlueCubesInSet * maxRedCubesInSet
      }
  ).sum

  println(result)
}