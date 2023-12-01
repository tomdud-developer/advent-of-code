package day1

object Main {
  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromFile("src/main/resources/day1/input.txt")

    def getFirstDigit(str: String): Int = str.toCharArray.find(_.isDigit).map(_.toInt).get //unsafe get
    def getLastDigit(str: String): Int = str.toCharArray.findLast(_.isDigit).map(_.toInt).get //unsafe get

    val result = file.getLines().map(line => s"${getFirstDigit(line)}${getLastDigit(line)}".toInt).sum

    println(result)
  }
}