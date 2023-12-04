package day4

object Part2 extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day4/input.txt")

  var cardsToCopy: Map[Int, Int] = Map.empty[Int, Int].withDefaultValue(0)
  val result = file.getLines().zipWithIndex.map { case (line, row) =>
    val cardNumber = row + 1
    val totalInstances = cardsToCopy(cardNumber) + 1
    val parts = line.split(':')(1).split('|').map(part => part.trim.split(' ').filter(_.nonEmpty).map(_.toInt))
    val matchingNumbers = (parts(0) ++ parts(1)).groupBy(identity).count(_._2.length == 2)

    (cardNumber + 1 to cardNumber + matchingNumbers).foreach(
      x => cardsToCopy = cardsToCopy.updatedWith(x)(_ map (_ + totalInstances) orElse Some(totalInstances))
    )

    totalInstances
  }.sum


  println(result)
}