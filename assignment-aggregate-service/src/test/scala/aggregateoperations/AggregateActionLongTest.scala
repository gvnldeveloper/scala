package aggregateoperations

import aggregateoperationservice.AggregateAction
import org.scalatest.FunSuite

class AggregateActionLongTest extends FunSuite {

  val aggregateActionLong: AggregateAction = new AggregateActionLong

  // Test for Sum Functionality
  test("testSumOfNullString") {
    assert(aggregateActionLong.sum("null") == Some(0))
  }

  test("testSumOfNull") {
    assert(aggregateActionLong.sum(null) == Some(0))
  }

  test("testSumOfValidLongInputs") {
    assert(aggregateActionLong.sum("1,1,2,3,4,5") == Some(16))
  }

  test("testSumOfSpaceAsInputs") {
    assert(aggregateActionLong.sum("1,2,  ,  2  , ") == Some(5))
  }

  test("testSumOfLongInputsWithPlusOperator") {
    assert(aggregateActionLong.sum("1,2,  ,  +2  , ") == Some(5))
  }

  test("testSumOfLongInputsWithMinusOperator") {
    assert(aggregateActionLong.sum("1,null, -12,2.22,  ,1.  2  ,2.  ") == Some(-11))
  }

  test("testSumOfLongWithSecondMaxInputs") {
    assert(aggregateActionLong.sum("9223372036854775806,1") == Some(9223372036854775807L))
  }

  test("testSumOfLongWithMaxInputs") {
    assert(aggregateActionLong.sum("9223372036854775807,1") == Some(-9223372036854775808L))
  }

  test("testSumOfLongWithMinInputs") {
    assert(aggregateActionLong.sum("-9223372036854775808,-1") == Some(9223372036854775807L))
  }

  test("testSumOfInputWithEmptyString") {
    assert(aggregateActionLong.sum("     ") == Some(0))
  }


  // Test for Max Functionality

  test("testMaxOfNullString") {
    assert(aggregateActionLong.max("null") == Some(None))
  }

  test("testMaxOfNull") {
    assert(aggregateActionLong.max(null) == Some(None))
  }

  test("testMaxOfValidLongInputs") {
    assert(aggregateActionLong.max("1,1,2,3,4,5") == Some(5))
  }

  test("testMaxOfSpaceAsInputs") {
    assert(aggregateActionLong.max("1,2,  ,  2  , ") == Some(2))
  }

  test("testMaxOfLongInputsWithPlusOperator") {
    assert(aggregateActionLong.max("1,2,  ,  +2  , ") == Some(2))
  }

  test("testMaxOfLongInputsWithMinusOperator") {
    assert(aggregateActionLong.max("1,null, -12,2.22,  ,1.  2  ,2.  ") == Some(1))
  }

  test("testMaxOfLongWithSecondMaxInputs") {
    assert(aggregateActionLong.max("9223372036854775806,1") == Some(9223372036854775806L))
  }

  test("testMaxOfLongWithMaxInputs") {
    assert(aggregateActionLong.max("2,3,4,5,-6,-7,87,9,9,0,0,9223372036854775807,1") == Some(9223372036854775807L))
  }

  test("testMaxOfLongWithMinInputs") {
    assert(aggregateActionLong.max("-9223372036854775808,-1") == Some(-1))
  }

  test("testMaxOfInputWithEmptyString") {
    assert(aggregateActionLong.max("     ") == Some(None))
  }

  // Test for Mean Functionality

  test("testMeanOfNullString") {
    assert(aggregateActionLong.mean("null") == Some(0.0))
  }

  test("testMeanOfNull") {
    assert(aggregateActionLong.mean(null) == Some(None))
  }

  test("testMeanOfValidLongInputs") {
    assert(aggregateActionLong.mean("1,1,2,3,4,5") == Some(2.6666666666666665))
  }

  test("testMeanOfSpaceAsInputs") {
    assert(aggregateActionLong.mean("1,2,  ,  2  , ") == Some(1.0))
  }

  test("testMeanOfLongInputsWithPlusOperator") {
    assert(aggregateActionLong.mean("1,2,  ,  +2  , ") == Some(1.0))
  }

  test("testMeanOfLongInputsWithMinusOperator") {
    assert(aggregateActionLong.mean("1,null, -12,2.22,  ,1.  2  ,2.  ") == Some(-1.5714285714285714))
  }

  test("testMeanOfLongWithSecondMaxInputs") {
    assert(aggregateActionLong.mean("9223372036854775806,1") == Some(4.6116860184273879E18))
  }

  test("testMeanOfLongWithMaxInputs") {
    assert(aggregateActionLong.mean("2,3,4,5,-6,-7,87,9,9,0,0,9223372036854775807,1") == Some(7.0949015668113664E17))
  }

  test("testMeanOfLongWithMinInputs") {
    assert(aggregateActionLong.mean("-9223372036854775808,-1") == Some(-4.6116860184273879E18))
  }

  test("testMeanOfInputWithEmptyString") {
    assert(aggregateActionLong.mean("     ") == Some(None))
  }


}

