package day6


object Part1 extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day6/input.txt")
  val lines = file.getLines().toList
  val times = lines(0).split(':')(1).trim.split(" ").filter(_.nonEmpty).map(_.toInt).toList
  val distances = lines(1).split(':')(1).trim.split(" ").filter(_.nonEmpty).map(_.toInt).toList
  val races = times.zip(distances)

  val result = races.map(race => getNumberOfPossibleWinningValues(race._1, race._2)).product

  println(result)

  def getNumberOfPossibleWinningValues(totalTime: Int, lengthMustBeGreaterThan: Int): Int = {
    val a = -1
    val b = totalTime
    val c = -lengthMustBeGreaterThan

    val sqrtDelta = Math.sqrt(b * b - 4 * a * c)
    val x1 = (-b + sqrtDelta) / (2 * a)
    val x2 = (-b - sqrtDelta) / (2 * a)

    (getIntBefore(x2) - getNextInt(x1) + 1).toInt
  }

  def getNextInt(double: Double): Int = {
    val epsilon = 1e-15
    val roundedUp = math.ceil(double).toInt
    if (math.abs(double - roundedUp) < epsilon) roundedUp + 1 else roundedUp
  }

  def getIntBefore(double: Double): Int = {
    val epsilon = 1e-15
    val roundedDown = math.floor(double).toInt
    if (math.abs(double - roundedDown) < epsilon) roundedDown - 1 else roundedDown
  }

}


