package aggregateoperations

import aggregateoperationservice.AggregateAction
import org.scalatest.FunSuite

class AggregateActionBigDecimalTest extends FunSuite {

  val aggregateActionBigDecimal: AggregateAction = new AggregateActionBigDecimal

  // Test for Sum Functionality

  test("testSumOfNullString") {
    assert(aggregateActionBigDecimal.sum("null") == Some(0.0))
  }

  test("testSumOfNull") {
    assert(aggregateActionBigDecimal.sum(null) == Some(0.0))
  }

  test("testSumOfSpaceAsInputs") {
    assert(aggregateActionBigDecimal.sum("1.9494,2.454,  ,  2  , ") == Some(6.4034))
  }

  test("testSumOfBigDecimalInputsWithPlusOperator") {
    assert(aggregateActionBigDecimal.sum("1,2.3,  ,  +2.5656  , ") == Some(5.8656))
  }

  test("testSumOfBigDecimalInputsWithMinusOperator") {
    assert(aggregateActionBigDecimal.sum("1.675,null, -12,2.22,  ,1.  2  ,2.  ") == Some(-8.105))
  }

  test("testSumOfBigDecimalWithSecondMaxInputs") {
    assert(aggregateActionBigDecimal.sum("1.79769313486231570E+307,1") == Some(1.0))
  }

  test("testSumOfBigDecimalWithMaxInputs") {
    assert(aggregateActionBigDecimal.sum("1.79769313486231570E+308,1") == Some(1.0))
  }

  test("testSumOfBigDecimalWithMinInputs") {
    assert(aggregateActionBigDecimal.sum("-1.79769313486231570E+308,-1") == Some(-1.0))
  }

  test("testSumOfInputWithEmptyString") {
    assert(aggregateActionBigDecimal.sum("     ") == Some(0.0))
  }


  // Test for Max Functionality


  test("testMaxOfNullString") {
    assert(aggregateActionBigDecimal.max("null") == Some(None))
  }

  test("testMaxOfNull") {
    assert(aggregateActionBigDecimal.max(null) == Some(None))
  }

  test("testMaxOfSpaceAsInputs") {
    assert(aggregateActionBigDecimal.max("1.9494,2.454,  ,  2  , ") == Some(2.454))
  }

  test("testMaxOfBigDecimalInputsWithPlusOperator") {
    assert(aggregateActionBigDecimal.max("1,2.3,  ,  +2.5656  , ") == Some(2.5656))
  }

  test("testMaxOfBigDecimalInputsWithMinusOperator") {
    assert(aggregateActionBigDecimal.max("1.675,null, -12,2.22,  ,1.  2  ,2.  ") == Some(2.22))
  }

  test("testMaxOfBigDecimalWithSecondMaxInputs") {
    assert(aggregateActionBigDecimal.max("1.79769313486231570E+307,1") == Some(1.0))
  }

  test("testMaxOfBigDecimalWithMaxInputs") {
    assert(aggregateActionBigDecimal.max("1.79769313486231570E+308,1") == Some(1.0))
  }

  test("testMaxOfBigDecimalWithMinInputs") {
    assert(aggregateActionBigDecimal.max("-1.79769313486231570E+308,-1") == Some(-1.0))
  }

  test("testMaxOfInputWithEmptyString") {
    assert(aggregateActionBigDecimal.max("     ") == Some(None))
  }


  // Test for Mean Functionality


  test("testMeanOfNullString") {
    assert(aggregateActionBigDecimal.mean("null") == Some(0.0))
  }

  test("testMeanOfNull") {
    assert(aggregateActionBigDecimal.mean(null) == Some(None))
  }

  test("testMeanOfSpaceAsInputs") {
    assert(aggregateActionBigDecimal.mean("1.9494,2.454,  ,  2  , ") == Some(1.28068))
  }

  test("testMeanOfBigDecimalInputsWithPlusOperator") {
    assert(aggregateActionBigDecimal.mean("1,2.3,  ,  +2.5656  , ") == Some(1.17312))
  }

  test("testMeanOfBigDecimalWithSecondMaxInputs") {
    assert(aggregateActionBigDecimal.mean("1.79769313486231570E+307,1") == Some(0.5))
  }

  test("testMeanOfBigDecimalWithMaxInputs") {
    assert(aggregateActionBigDecimal.mean("1.79769313486231570E+308,1") == Some(0.5))
  }

  test("testMeanOfBigDecimalWithMinInputs") {
    assert(aggregateActionBigDecimal.mean("-1.79769313486231570E+308,-1") == Some(-0.5))
  }

  test("testMeanOfInputWithEmptyString") {
    assert(aggregateActionBigDecimal.mean("     ") == Some(None))
  }


}
