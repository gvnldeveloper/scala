package aggregateoperations

import aggregateoperationservice.AggregateAction
import org.scalatest.FunSuite

class AggregateActionDoubleTest extends FunSuite {

  val aggregateActionDouble: AggregateAction = new AggregateActionDouble

  // Test for Sum Functionality

  test("testSumOfNullString") {
    assert(aggregateActionDouble.sum("null") == Some(0.0))
  }

  test("testSumOfNull") {
    assert(aggregateActionDouble.sum(null) == Some(0.0))
  }

  test("testSumOfValidDoubleInputs") {
    assert(aggregateActionDouble.sum("1.222,1.33333,21.109090909099899999999,3.23333311,12345514.4444444444,0.25") == Some(1.2345541592198463E7))
  }

  test("testSumOfSpaceAsInputs") {
    assert(aggregateActionDouble.sum("1.9494,2.454,  ,  2  , ") == Some(6.4034))
  }

  test("testSumOfDoubleInputsWithPlusOperator") {
    assert(aggregateActionDouble.sum("1,2.3,  ,  +2.5656  , ") == Some(5.8656))
  }

  test("testSumOfDoubleInputsWithMinusOperator") {
    assert(aggregateActionDouble.sum("1.675,null, -12,2.22,  ,1.  2  ,2.  ") == Some(-8.104999999999999))
  }

  test("testSumOfDoubleWithSecondMaxInputs") {
    assert(aggregateActionDouble.sum("1.79769313486231570E+307,1") == Some(1.0))
  }

  test("testSumOfDoubleWithMaxInputs") {
    assert(aggregateActionDouble.sum("1.79769313486231570E+308,1") == Some(1.0))
  }

  test("testSumOfDoubleWithMinInputs") {
    assert(aggregateActionDouble.sum("-1.79769313486231570E+308,-1") == Some(-1.0))
  }

  test("testSumOfInputWithEmptyString") {
    assert(aggregateActionDouble.sum("     ") == Some(0.0))
  }


  // Test for Max Functionality


  test("testMaxOfNullString") {
    assert(aggregateActionDouble.max("null") == Some(None))
  }

  test("testMaxOfNull") {
    assert(aggregateActionDouble.max(null) == Some(None))
  }

  test("testMaxOfValidDoubleInputs") {
    assert(aggregateActionDouble.max("1.222,1.33333,21.109090909099899999999,3.23333311,12345514.4444444444,0.25") == Some(1.2345514444444444E7))
  }

  test("testMaxOfSpaceAsInputs") {
    assert(aggregateActionDouble.max("1.9494,2.454,  ,  2  , ") == Some(2.454))
  }

  test("testMaxOfDoubleInputsWithPlusOperator") {
    assert(aggregateActionDouble.max("1,2.3,  ,  +2.5656  , ") == Some(2.5656))
  }

  test("testMaxOfDoubleInputsWithMinusOperator") {
    assert(aggregateActionDouble.max("1.675,null, -12,2.22,  ,1.  2  ,2.  ") == Some(2.22))
  }

  test("testMaxOfDoubleWithSecondMaxInputs") {
    assert(aggregateActionDouble.max("1.79769313486231570E+307,1") == Some(1.0))
  }

  test("testMaxOfDoubleWithMaxInputs") {
    assert(aggregateActionDouble.max("1.79769313486231570E+308,1") == Some(1.0))
  }

  test("testMaxOfDoubleWithMinInputs") {
    assert(aggregateActionDouble.max("-1.79769313486231570E+308,-1") == Some(-1.0))
  }

  test("testMaxOfInputWithEmptyString") {
    assert(aggregateActionDouble.max("     ") == Some(None))
  }


  // Test for Mean Functionality


  test("testMeanOfNullString") {
    assert(aggregateActionDouble.mean("null") == Some(0.0))
  }

  test("testMeanOfNull") {
    assert(aggregateActionDouble.mean(null) == Some(None))
  }

  test("testMeanOfValidDoubleInputs") {
    assert(aggregateActionDouble.mean("1.222,1.33333,21.109090909099899999999,3.23333311,12345514.4444444444,0.25") == Some(2057590.2653664106))
  }

  test("testMeanOfSpaceAsInputs") {
    assert(aggregateActionDouble.mean("1.9494,2.454,  ,  2  , ") == Some(1.28068))
  }

  test("testMeanOfDoubleInputsWithPlusOperator") {
    assert(aggregateActionDouble.mean("1,2.3,  ,  +2.5656  , ") == Some(1.17312))
  }

  test("testMeanOfDoubleInputsWithMinusOperator") {
    assert(aggregateActionDouble.mean("1.675,null, -12,2.22,  ,1.  2  ,2.  ") == Some(-1.1578571428571427))
  }

  test("testMeanOfDoubleWithSecondMaxInputs") {
    assert(aggregateActionDouble.mean("1.79769313486231570E+307,1") == Some(0.5))
  }

  test("testMeanOfDoubleWithMaxInputs") {
    assert(aggregateActionDouble.mean("1.79769313486231570E+308,1") == Some(0.5))
  }

  test("testMeanOfDoubleWithMinInputs") {
    assert(aggregateActionDouble.mean("-1.79769313486231570E+308,-1") == Some(-0.5))
  }

  test("testMeanOfInputWithEmptyString") {
    assert(aggregateActionDouble.mean("     ") == Some(None))
  }


}
