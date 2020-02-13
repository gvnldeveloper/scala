package aggregateoperationtailrecursion

import util.CommonUtils

import scala.annotation.tailrec

class AggregateActionLong {


  def max(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
      return Option(None)

    val list: List[Long] = values.split(",")
      .map(_.trim)
      .filter(x => x.matches("[\\+\\-]?\\d+"))
      .map(_.toLong)
      .toList

    @tailrec
    def maxWithTailRec(ints: List[Long], theMax: Long): Option[Any] = {
      if (ints.isEmpty) {
        Option(theMax)
      } else {
        val newMax = if (ints.head > theMax) ints.head else theMax
        maxWithTailRec(ints.tail, newMax)
      }
    }

    maxWithTailRec(list, 0)
  }

  def mean(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
      return Option(0)
    Some(sum(values).get.toString.toDouble / values.split(",").toList.size)

  }

  def sum(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
    return Option(0)

    val list: List[Long] = values.split(",")
      .map(_.trim)
      .filter(x => x.matches("[\\+\\-]?\\d+"))
      .map(_.toLong)
      .toList


    @tailrec
    def sumWithTailRec(list: List[Long], currentSum: Long): Option[Any] = {
      list match {
        case Nil => Option(currentSum)
        case x :: xs => sumWithTailRec(xs, currentSum + x)
      }
    }

    sumWithTailRec(list, 0)
  }
}

