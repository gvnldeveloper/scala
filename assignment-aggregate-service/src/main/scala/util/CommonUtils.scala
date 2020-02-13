package util

object CommonUtils {

  def isEmpty(input:String): Boolean=
  {
    input == null || input.trim.isEmpty
  }

}
