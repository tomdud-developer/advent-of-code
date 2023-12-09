package day8

import scala.annotation.tailrec

object Part1 extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day8/input.txt")
  val lines = file.getLines().toList
  val turns = lines.head
  val navigator = new Navigator(turns)

  val network: Map[String, (String,String)] = lines.drop(2)
    .map(line => {
      val splitted = line.split("=")
      val node = splitted(0).trim
      val ways = splitted(1).trim
        .replace("(", "")
        .replace(")", "")
        .split(",")
        .map(_.trim) match {
          case Array(l, r) => (l, r)
        }

      (node, ways)
    }).toMap

  val count = moveToZZZFrom("AAA")

  println(count)


  @tailrec
  def moveToZZZFrom(node: String, acc: Long = 0): Long = {
    node match {
      case "ZZZ" => acc
      case _ =>
        val possibleTurns = network(node)
        val nextTurn = navigator.next()
        val nextNode = if (nextTurn == 'L') possibleTurns._1 else possibleTurns._2
        moveToZZZFrom(nextNode, acc + 1)
    }
  }
  class Navigator(turnsString: String) {

    private val turns = turnsString.split("").map(_.charAt(0))
    private val length = turns.length
    private var current = -1

    def next(): Char = {
      if (current >= length - 1) current = -1

      current += 1
      turns(current)
    }

  }

}







