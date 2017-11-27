/**
  * Justin Crawford
  * COSC 455
  * Project 2
  */

class ScalaWarmup {

  // This method takes in an Int and returns T/F based on whether
  // the parameter passed is a prime number
  def prime(n: Int): Boolean = {
    n < 2 match {
      case true => false
      case false => !2.until(n - 1).exists(n % _ == 0)
    }
  }

  //prime(5)

  // This method takes in 2 Ints and returns T/F based on whether
  // the parameters passed are twin primes
  def twinPrimes(n1: Int, n2: Int): Boolean = {
    n1 - n2 match {
      case _ => false
      case 2 => prime(n1) && prime(n2)
      case -2 => prime(n1) && prime(n2)
    }
  }

  //twinPrimes(6, 8)

  // This method takes in an Int and returns a list of Ints.
  // The list is comprised of prime numbers going up to the passed parameter
  def twinPrimesList(n: Int): List[Int] = {
    List.range(2, n + 1) filter {
      x => twinPrimes(x, x + 2) || twinPrimes(x, x - 2)
    }
  }

  //twinPrimesList(25)

  // This method takes an Int and prints out a solution that
  // satisfies Goldbach's conjecture
  def goldbach (n : Int) = {
    (checkEven(n), n > 2) match {
      case (true, true) => goldbachHelper(n, 1 to n toList)
      case (true, false) => printf("Number must be greater than 2!")
      case (false, true) => printf("Number is not even. Number must be even!")
      case (false, false) => printf("Number is not even and is less than 2. Number must be even and greater than 2!")
    }
  }

  // Helper method for goldbach
  // This method handles the method output based on what parameters are passed
  def goldbachHelper (n : Int, myList : List[Int]) : Unit = {
    myList.reverse.filter(x => prime(x)) match {
      case head :: _ =>
        if (myList.contains(n - head)) {
          if (true) {
            printf(n - head + " + " + head + " = " + n + "\n")
          }
          else {
            Nil
          }
        }
      case _ => printf("(° ͜ʖ °)")
    }
  }

  // Helper method for goldbach
  // This method checks if the integer passed to it is an even number
  def checkEven(n : Int) : Boolean = {
    n % 2 match {
      case _ => false
      case 0 => true
    }
  }

  goldbach(25)
}
