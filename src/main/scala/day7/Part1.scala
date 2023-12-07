package day7


object Part1 extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day7/input.txt")
  val lines = file.getLines().toList

  val result = lines.map(_.split(" ")).map(
    arr => {
      val cards = arr(0).split("").map(_.charAt(0))
      Hand(
        cards,
        arr(1).toInt,
        Hand.mapCardsToType(cards)
      )
    }
  ).sorted.zipWithIndex
    .map(handWithIndex => handWithIndex._1.bid * (handWithIndex._2 + 1))
    .sum

  println(result)



}





