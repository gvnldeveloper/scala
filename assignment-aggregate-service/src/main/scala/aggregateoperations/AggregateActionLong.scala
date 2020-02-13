package aggregateoperations

import aggregateoperationservice.AggregateAction
import util.CommonUtils

class AggregateActionLong extends AggregateAction {

  @Override
  def sum(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
      return Option(0)

    Some(
      values.split(",")
        .map(_.trim)
        .filter(x => x.matches("[\\+\\-]?\\d+"))
        .map(_.toLong)
        .sum).orElse(Option(0))

  }

  @Override
  def max(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
      return Option(None)

    val res: Array[Long] = Some(
      values.split(",")
        .map(_.trim)
        .filter(x => x.matches("[\\+\\-]?\\d+"))
        .map(_.toLong)).get

    if (res.isEmpty)
      Option(None)
    else
      Some(res.max).orElse(Option(None))

  }

  @Override
  def mean(values: String): Option[Any] = {
    if (CommonUtils.isEmpty(values))
      return Option(None)

    val input = values.split(",")
    val sumo: Option[Any] = Some(
      input
        .map(_.trim)
        .filter(x => x.matches("[\\+\\-]?\\d+"))
        .map(_.toDouble)
        .sum).orElse(Option(None))

    Some(sumo.getOrElse(None).asInstanceOf[Double] / input.length)


  }

}

