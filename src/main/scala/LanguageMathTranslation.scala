/**
  * Justin Crawford
  * COSC 455
  * Project 2
  */

class LanguageMathTranslation {

  def myMember (s : String, myList : List[String]) : Boolean = {
    myList match {
      case Nil => false
      case listHead :: listTail =>
        if (s == listHead)
          true
        else
          myMember(s, listTail)
    }
  }

  def translate (myList : List[String]) : List[Int] = {
    myList match {
      case Nil => Nil
      case listHead :: listTail =>
        if (english.contains(listHead))
          english.indexOf(listHead) :: translate(listTail)
        else
          chinese.indexOf(listHead) :: translate(listTail)
    }
  }

  def go (myList : List[String]) : String = {
    myList match {
      case Nil => "List is Empty"
      case aList => {
        val newList = translate(filter(aList))
        val a = add(newList)
        val m = multi(newList)

        "Translation " + newList.mkString(" ") +
          "\nAddition " + newList.mkString(" + ") + " = " + a +
          "\nMultiplication " + newList.mkString(" * ") + " = " + m
      }
    }
  }

  // ===== Helper Methods =====

  def add (myList : List[Int]) : Int = {
    myList.foldLeft(0)(_+_)
  }

  def multi (myList : List[Int]) : Int = {
    myList.foldLeft(1)(_*_)
  }

  def filter (myList : List[String]) = {
    myList.filter(x => myMember(x, LANG))
  }

  // ===== Lists =====

  final val LANG = chinese ::: english

  val english : List[String] = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")

  val chinese : List[String] = List("ling", "yi", "er", "san", "si", "wu", "liu", "qi", "ba", "jiu", "shi")

  // ========== Test Cases ==========

  val test : List[String] = List("ling", "five", "er", "four", "san", "six", "er")

  go(test)
}
