package day4

object Part1 extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day4/input.txt")

  val result = file.getLines().map(_.split(':')(1).split('|').map(part => part.trim.split(' ').filter(_.nonEmpty)))
    .map(p => (p(0) ++ p(1)).groupBy(identity).count(_._2.length == 2)).filter(_ > 0).map(x => Math.pow(2, x-1)).sum

  println(result)
}