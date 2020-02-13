package aggregateoperationtailrecursion

import util.CommonUtils

import scala.annotation.tailrec

class AggregateActionBigDecimal {


  def max(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
      return Option(None)

    val list: List[BigDecimal] = values.split(",")
      .map(_.trim)
      .filter(x => x.matches("^[-+]?\\d+(\\.{0,1}(\\d+?))?$"))
      .map(x => BigDecimal(x))
      .toList

    @tailrec
    def maxWithTailRec(ints: List[BigDecimal], theMax: BigDecimal): Option[Any] = {
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
      return Option(0.0)
    Some(sum(values).getOrElse(None).asInstanceOf[BigDecimal] / values.split(",").toList.size)

  }

  def sum(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
      return Option(0.0)

    val list: List[BigDecimal] = values.split(",")
      .map(_.trim)
      .filter(x => x.matches("^[-+]?\\d+(\\.{0,1}(\\d+?))?$"))
      .map(x => BigDecimal(x))
      .toList


    @tailrec
    def sumWithTailRec(list: List[BigDecimal], currentSum: BigDecimal): Option[Any] = {
      list match {
        case Nil => Option(currentSum)
        case x :: xs => sumWithTailRec(xs, currentSum + x)
      }
    }

    sumWithTailRec(list, 0)
  }


  println("val:" + sum("1.0,2.0,3.0,4.0"))

  println("val:" + sum("null"))
  println("val:" + sum(null))
  println("val:" + sum("1.222,1.33333,21.109090909099899999999,3.23333311,12345514.4,0.25"))
  println("val:" + sum("1.9494,2.454,  ,  2  , "))
  println("val:" + sum("1,2.3,  ,  +2.5656  , "))
  println("val:" + sum("1.675,null, -12,2.22,  ,1.  2  ,2.  "))
  println("val:" + sum("1.79769313486231570E+307,1")) // one less than highest
  println("val:" + sum("1.79769313486231570E+308,1")) // highest range with 1
  println("val:" + sum("-1.79769313486231570E+308,-1")) // lowest range with -1
  println("val:" + sum("     "))


  println("val:" + max("1.0,2.0,3.0,4.0"))

  println("val:" + max("null"))
  println("val:" + max(null))
  println("val:" + max("1.222,1.33333,21.109090909099899999999,3.23333311,12345514.4,0.25"))
  println("val:" + max("1.9494,2.454,  ,  2  , "))
  println("val:" + max("1,2.3,  ,  +2.5656  , "))
  println("val:" + max("1.675,null, -12,2.22,  ,1.  2  ,2.  "))
  println("val:" + max("1.79769313486231570E+307,1")) // one less than highest
  println("val:" + max("1.79769313486231570E+308,1")) // highest range with 1
  println("val:" + max("-1.79769313486231570E+308,-1")) // lowest range with -1
  println("val:" + max("     "))


  println("val:" + mean("1.0,2.0,3.0,4.0"))

  println("val:" + mean("null"))
  println("val:" + mean(null))
  println("val:" + mean("1.222,1.33333,21.109090909099899999999,3.23333311,12345514.4,0.25"))
  println("val:" + mean("1.9494,2.454,  ,  2  , "))
  println("val:" + mean("1,2.3,  ,  +2.5656  , "))
  println("val:" + mean("1.675,null, -12,2.22,  ,1.  2  ,2.  "))
  println("val:" + mean("1.79769313486231570E+307,1")) // one less than highest
  println("val:" + mean("1.79769313486231570E+308,1")) // highest range with 1
  println("val:" + mean("-1.79769313486231570E+308,-1")) // lowest range with -1
  println("val:" + mean("     "))


}

