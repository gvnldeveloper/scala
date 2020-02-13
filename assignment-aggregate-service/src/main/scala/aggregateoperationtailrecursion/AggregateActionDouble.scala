package aggregateoperationtailrecursion

import util.CommonUtils

import scala.annotation.tailrec

class AggregateActionDouble {


  def max(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
      return Option(None)

    val list: List[Double] = values.split(",")
      .map(_.trim)
      .filter(x => x.matches("^[-+]?\\d+(\\.{0,1}(\\d+?))?$"))
      .map(_.toDouble)
      .toList

    @tailrec
    def maxWithTailRec(ints: List[Double], theMax: Double): Option[Any] = {
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
      return Option(0.0)

    val list: List[Double] = values.split(",")
      .map(_.trim)
      .filter(x => x.matches("^[-+]?\\d+(\\.{0,1}(\\d+?))?$"))
      .map(_.toDouble)
      .toList


    @tailrec
    def sumWithTailRec(list: List[Double], currentSum: Double): Option[Any] = {
      list match {
        case Nil => Option(currentSum)
        case x :: xs => sumWithTailRec(xs, currentSum + x)
      }
    }

    sumWithTailRec(list, 0)
  }


}

