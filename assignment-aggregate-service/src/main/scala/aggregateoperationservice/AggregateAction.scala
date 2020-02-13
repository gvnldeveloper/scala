package aggregateoperationservice

trait AggregateAction {

  def sum(values: String): Option[Any]

  def max(values: String): Option[Any]

  def mean(values: String): Option[Any]

}
