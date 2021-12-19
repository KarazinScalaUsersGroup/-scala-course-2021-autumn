package karazin.scala.users.group.week1.homework

import scala.annotation.tailrec

/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean =
      if b then false
      else true

    def and(left: Boolean, right: Boolean): Boolean =
      if left == right then true
      else false

    def or(left: Boolean, right: Boolean): Boolean =
      if left then true
      else if right then true
      else false

  end `Boolean Operators`

  object `Tail Recursion`:

    def multiplication(first: BigInt, second: BigInt): BigInt =
      @tailrec // Optional annotation, should be imported (review imports at the beggining of the file)
      def multiplicationRec(first: BigInt, second: BigInt, result: BigInt): BigInt =
        if `Boolean Operators`.or(first == 0, second == 0) then result
        else multiplicationRec(first - 1, second, result + second)
      multiplicationRec(first, second, result = 0)

    def power(number: BigInt, degree: BigInt): BigInt =
      @tailrec
      def powerRec(number: BigInt, degree: BigInt, result: BigInt): BigInt =
        if degree == 0 then result
        else powerRec(number, degree - 1, multiplication(number, result))
      powerRec(number, degree, result = 1)

  end `Tail Recursion`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) => BigInt = `Tail Recursion`.multiplication

    val power: (BigInt, BigInt) => BigInt = `Tail Recursion`.power

    val fermatNumber: Int => BigInt = v => power(2, power(2, v)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence` :
    val lookAndSaySequenceElement: Int => BigInt = countAndSay

    def countAndSay(n: Int): BigInt = {
      if (n == 1) return 1
      else if (n == 2) return 11
      BigInt(findElementN("11", n, 3))
    }

    def findElementN(str: String, n: Int, i: Int): String = {
      var answer = str
      if (i <= n) {
        answer = findNextElement((str + '$').toCharArray, 1, "", 1)
        answer = findElementN(answer, n, i + 1)
      }
      answer
    }

    def findNextElement(arr: Array[Char], cnt: Int, tmp: String, counter: Int): String = {
      var answer = tmp
      if (counter < arr.length) if (arr(counter) != arr(counter - 1)) answer = findNextElement(arr, 1, (tmp + cnt) + arr(counter - 1), counter + 1)
      else answer = findNextElement(arr, cnt + 1, tmp, counter + 1)
      answer
    }

  end `Look-and-say Sequence`
  
  def main (args: Array[String]): Unit = {
    /*println(`Boolean Operators`.not(true))
    println(`Boolean Operators`.not(false))
    
    println()
    
    println(`Boolean Operators`.or(true, true))
    println(`Boolean Operators`.or(true, false))
    println(`Boolean Operators`.or(false, true))
    println(`Boolean Operators`.or(false, false))

    println()
    
    println(`Boolean Operators`.and(true, true))
    println(`Boolean Operators`.and(true, false))
    println(`Boolean Operators`.and(false, true))
    println(`Boolean Operators`.and(false, false))*/
    
/*    println(`Fermat Numbers`.multiplication(7, 9))
    println(`Fermat Numbers`.power(2, 11))
    println(`Fermat Numbers`.fermatNumber(3))*/

/*    println(`Look-and-say Sequence`.lookAndSaySequenceElement(4))
    println(`Look-and-say Sequence`.lookAndSaySequenceElement(5))
    println(`Look-and-say Sequence`.lookAndSaySequenceElement(20))*/
    
  }
  

end Homework

