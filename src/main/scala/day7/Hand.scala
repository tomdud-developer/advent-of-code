package day7

import day7.HandType.{FIVE_OF_A_KIND, FOUR_OF_A_KIND, FULL_HOUSE, HIGH_CARD, HandType, ONE_PAIR, THREE_OF_A_KIND, TWO_PAIR}

object Hand {
  def mapCardsToType(cards: Array[Char]): HandType = {
    val sortedCounts = cards.groupBy(identity).map(_._2.length).toArray.sorted.reverse
    sortedCounts match {
      case Array(5) => FIVE_OF_A_KIND
      case Array(4, 1) => FOUR_OF_A_KIND
      case Array(3, 2) => FULL_HOUSE
      case Array(3, 1, 1) => THREE_OF_A_KIND
      case Array(2, 2, 1) => TWO_PAIR
      case Array(2, 1, 1, 1) => ONE_PAIR
      case Array(1, 1, 1, 1, 1) => HIGH_CARD
      case _ => throw new IllegalStateException(s"Hand ${cards.mkString("Array(", ", ", ")")} not recognized")
    }
  }

  private val cardValues = Seq('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
  val cardValuesMap: Map[Char, Int] = cardValues.zipWithIndex.map {
    case (card, index) => (card, cardValues.length - index)
  }.toMap

}

case class Hand(
                 cards: Array[Char],
                 bid: Int,
                 handType: HandType.HandType
               ) extends Ordered[Hand] {
  override def compare(that: Hand): Int = {
    if (this.handType > that.handType) return 1
    else if (this.handType < that.handType) return -1

    val thisCardValues = this.cards.map(Hand.cardValuesMap(_))
    val thatCardValues = that.cards.map(Hand.cardValuesMap(_))

    val winner = thisCardValues.zip(thatCardValues).find(
      cardValuesPair => cardValuesPair._1 != cardValuesPair._2
    ).map(
      cardValuesPair => if (cardValuesPair._1 > cardValuesPair._2) this else that
    ).getOrElse(that)

    if (winner.equals(that)) -1 else 1
  }
}

object HandType extends Enumeration {
  type HandType = Value

  val FIVE_OF_A_KIND: day7.HandType.Value = Value(7, "Five of a Kind")
  val FOUR_OF_A_KIND: day7.HandType.Value = Value(6, "Four of a Kind")
  val FULL_HOUSE: day7.HandType.Value = Value(5, "Full House")
  val THREE_OF_A_KIND: day7.HandType.Value = Value(4, "Three of a Kind")
  val TWO_PAIR: day7.HandType.Value = Value(3, "Two Pair")
  val ONE_PAIR: day7.HandType.Value = Value(2, "One Pair")
  val HIGH_CARD: day7.HandType.Value = Value(1, "High Card")
}

