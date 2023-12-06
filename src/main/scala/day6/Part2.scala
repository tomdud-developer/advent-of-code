package day6


object Part2 extends App {
  private val EPSILON = 1e-15

  val file = scala.io.Source.fromFile("src/main/resources/day6/input2.txt")
  val lines = file.getLines().toList
  val times = lines(0).split(':')(1).trim.split(" ").filter(_.nonEmpty).map(_.toLong).toList
  val distances = lines(1).split(':')(1).trim.split(" ").filter(_.nonEmpty).map(_.toLong).toList
  val race = times.zip(distances).head

  val result = getNumberOfPossibleWinningValues(race._1, race._2)

  println(result)

  def getNumberOfPossibleWinningValues(totalTime: Long, lengthMustBeGreaterThan: Long): Long = {
    val a = -1
    val b = totalTime
    val c = -lengthMustBeGreaterThan

    val sqrtDelta = Math.sqrt(b * b - 4 * a * c)
    val x1 = (-b + sqrtDelta) / (2 * a)
    val x2 = (-b - sqrtDelta) / (2 * a)

    getIntBefore(x2) - getNextInt(x1) + 1L
  }

  def getNextInt(double: Double): Long = {
    val roundedUp = math.ceil(double).toInt
    if (math.abs(double - roundedUp) < EPSILON) roundedUp + 1 else roundedUp
  }

  def getIntBefore(double: Double): Long = {
    val roundedDown = math.floor(double).toInt
    if (math.abs(double - roundedDown) < EPSILON) roundedDown - 1 else roundedDown
  }

}


