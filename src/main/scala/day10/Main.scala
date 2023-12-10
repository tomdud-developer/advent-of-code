package day10

import scala.annotation.tailrec


object Main extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day10/input.txt")
  val lines = file.getLines().toList
  val map = lines.map(_.split("").map(_.charAt(0)).toArray).toArray
  val startRowIndex = map.zipWithIndex.find(t => t._1.contains('S')).get._2
  val startColumnIndex = map(startRowIndex).zipWithIndex.find(t => t._1.equals('S')).get._2

  println(goLoop((startRowIndex, startColumnIndex), (startRowIndex, startColumnIndex-1)) / 2)


  @tailrec
  def goLoop(previous: (Int, Int), current: (Int, Int), acc: Int = 0): Int = {
    println(getPipe(current))
    getPipe(current) match {
      case '-' =>
        val next = (current._1, current._2 + (if (previous._2 > current._2) -1 else 1))
        if (previous._2 > current._2) {
          if (!List('-', 'L', 'F', 'S').contains(getPipe(next))) return -1
        } else {
          if (!List('-', '7', 'J', 'S').contains(getPipe(next))) return -1
        }
        goLoop(current, next, acc + 1)
      case '|' =>
        val next = (current._1 + (if (previous._1 > current._1) -1 else 1), current._2)
        if (previous._1 > current._1) {
          if (!List('|', '7', 'F', 'S').contains(getPipe(next))) return -1
        } else {
          if (!List('|', 'J', 'L', 'S').contains(getPipe(next))) return -1
        }
        goLoop(current, next, acc + 1)
      case 'F' =>
        if (previous._2 != current._2) {
          val next = (current._1 + 1, current._2)
          if (List('|', 'J', 'L', 'S').contains(getPipe(next)))
            return goLoop(current, next, acc + 1)
          else return -1
        } else {
          val next = (current._1, current._2 + 1)
          if (List('-', 'J', '7', 'S').contains(getPipe(next)))
            return goLoop(current, next, acc + 1)
          else return -1
        }
      case '7' =>
        if (previous._2 != current._2) {
          val next = (current._1 + 1, current._2)
          if (List('|', 'J', 'L', 'S').contains(getPipe(next)))
            return goLoop(current, next, acc + 1)
          else return -1
        } else {
          val next = (current._1, current._2 - 1)
          if (List('-', 'L', 'F', 'S').contains(getPipe(next)))
            return goLoop(current, next, acc + 1)
          else return -1
        }
      case 'L' =>
        if (previous._2 != current._2) {
          val next = (current._1 - 1, current._2)
          if (List('|', 'F', '7', 'S').contains(getPipe(next)))
            return goLoop(current, next, acc + 1)
          else return -1
        } else {
          val next = (current._1, current._2 + 1)
          if (List('-', '7', 'J').contains(getPipe(next)))
            return goLoop(current, next, acc + 1)
          else return -1
        }
      case 'J' =>
        if (previous._2 != current._2) {
          val next = (current._1 - 1, current._2)
          if (List('|', 'F', '7', 'S').contains(getPipe(next)))
            return goLoop(current, next, acc + 1)
          else return -1
        } else {
          val next = (current._1, current._2 - 1)
          if (List('-', 'L', 'F', 'S').contains(getPipe(next)))
            return goLoop(current, next, acc + 1)
          else return -1
        }
      case 'S' => acc + 1
      case _ => -1
    }
  }

  def getPipe(rowCol: (Int, Int)): Char = map(rowCol._1)(rowCol._2)

}







