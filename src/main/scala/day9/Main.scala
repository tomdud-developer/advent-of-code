package day9

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Main extends App {

  val file = scala.io.Source.fromFile("src/main/resources/day9/input.txt")
  val lines = file.getLines().toList
  val histories = lines.map(_.split(" ").map(_.toLong));

  part2()

  def part1(): Unit = {
    val result = histories.map(_.toSeq).map(seq => genereatePiramid(seq).map(_.last).sum).sum
    print(result)
  }

  def part2(): Unit = {
    val result = histories.map(_.toSeq).map(seq => genereatePiramid(seq).map(_.head))
      .map(historySeqHeads => historySeqHeads.reverse.fold(0L)((acc, h) => h - acc)).sum

    println(result)

  }



  @tailrec
  def genereatePiramid(sequence: Seq[Long], acc: ListBuffer[Seq[Long]] = ListBuffer.empty[Seq[Long]]): ListBuffer[Seq[Long]] = {
    sequence match {
      case seq if seq.exists(x => x != 0L) =>
        acc += seq
        val newSequence = seq.sliding(2, 1).map(pair => pair(1) - pair.head).toSeq
        genereatePiramid(newSequence, acc)
      case seq =>
        acc += seq
        acc
    }
  }



}







