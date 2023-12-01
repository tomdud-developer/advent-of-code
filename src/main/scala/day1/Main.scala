package day1

import scala.collection.immutable.Map

object Main {
  def main(args: Array[String]): Unit = {
    val file = scala.io.Source.fromFile("src/main/resources/day1/sample2.txt")

    def getTextDigit(str: String): Option[Int] = {
      Map(
        1 -> "one",
        2 -> "two",
        3 -> "three",
        4 -> "four",
        5 -> "five",
        6 -> "six",
        7 -> "seven",
        8 -> "eight",
        9 -> "nine"
      ).find(digit => str.contains(digit._2)).map(_._1)
    }

    def getSlicesFromBegin(str: String) = (1 to str.length).map(i => str.slice(0, i))
    def getSlicesFromEnd(str: String) = (1 to str.length).map(i => str.slice(str.length - i, str.length))

    def getFirstDigit(str: String): Int =
      getSlicesFromBegin(str).map(slice => {
          if (slice.last.isDigit) {
            Some(slice.last.asDigit)
          } else {
            getTextDigit(slice)
          }
        })
        .find(_.isDefined).get.get // not elegant

    def getLastDigit(str: String): Int =
      getSlicesFromEnd(str).map(slice => {
          println("slice: " + slice)
          if (slice.head.isDigit) {
            Some(slice.head.asDigit)
          } else {
            getTextDigit(slice)
          }
        })
        .find(_.isDefined).get.get // not elegant


    val result = file.getLines().map(line => {
      s"${getFirstDigit(line)}${getLastDigit(line)}".toInt
    }).sum

    println(result)
  }
}