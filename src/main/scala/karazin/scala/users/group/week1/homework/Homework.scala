package karazin.scala.users.group.week1.homework

import scala.::
import scala.annotation.tailrec
import scala.language.postfixOps

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
 * c) the function should use if expression and true and false boolean literals 
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


object Homework:

  object `Boolean Operators`:

    def not(b: Boolean): Boolean =
      if b then false
      else true


    def and(left: Boolean, right: Boolean): Boolean =
      (left, right) match {
        case (true, true) => true
        case (true, false) => false
        case (false, true) => false
        case (false, false) => false
      }


    def or(left: Boolean, right: Boolean): Boolean =
      (left, right) match {
        case (true, true) => true
        case (true, false) => true
        case (false, true) => true
        case (false, false) => false
      }


  end `Boolean Operators`

  object `Fermat Numbers`:

    val multiplication: (BigInt, BigInt) => BigInt =
      (a, b) =>
        @tailrec
        def multiplicationReq(a: BigInt, b: BigInt, sum: BigInt): BigInt =
          if b == 0 then sum
          else multiplicationReq(a, b - 1, sum + a)

        multiplicationReq(a, b, sum = 0)

    val power: (BigInt, BigInt) => BigInt =
      (a, b) =>
        @tailrec
        def powerReq(a: BigInt, b: BigInt, ans: BigInt): BigInt =
          if b == 0 then ans
          else powerReq(a, b - 1, ans * a)

        powerReq(a, b, ans = 1)

    val fermatNumber: Int => BigInt =
      n =>
        power(2, power(2, n)) + 1

  end `Fermat Numbers`

  object `Look-and-say Sequence`:
    val lookAndSaySequenceElement: Int => BigInt =
      n =>
        @tailrec
        def lookAndSaySequenceElementReq(n: BigInt, calc: BigInt => BigInt, ans: BigInt): BigInt =
          if n == 0 then ans
          else
            val gt = calc(ans)
            lookAndSaySequenceElementReq(n - 1, calc, gt)

        @tailrec
        def fReq(x: BigInt, pos: BigInt, last: List[BigInt], last2: List[BigInt]): List[BigInt] =
          if pos == (x.toString().length) then last
          else if pos == 0 then
            fReq(x, pos + 1, (last :+ BigInt(1)) :+ (x.toString()(pos.toInt).toInt - '0'.toInt), last)
          else if (x.toString()((pos - 1).toInt).toInt - '0'.toInt) == (x.toString()(pos.toInt).toInt - '0'.toInt) then
            fReq(x, pos + 1, (last2 :+ (last(last.length - 2) + BigInt(1))) :+ (x.toString()(pos.toInt).toInt - '0'.toInt), last2)
          else
            fReq(x, pos + 1, (last :+ BigInt(1)) :+ (x.toString()(pos.toInt).toInt - '0'.toInt), last)

        @tailrec
        def fromListToString(list: List[BigInt], ans: String, ind: Int): String =
          if ind == list.length then ans
          else fromListToString(list, ans + list(ind).toString, ind + 1)


        def f(x: BigInt): BigInt =
          val dr = fReq(x, 0, List.empty, List.empty)
//          println(fromListToString(dr, "", 0))
          BigInt(fromListToString(dr, "", 0))

        lookAndSaySequenceElementReq(n-1, f, 1)
  end `Look-and-say Sequence`

end Homework

object Main {
  def main(args: Array[String]) = {
    for( a <- 1 to 100)
      println(Homework.`Look-and-say Sequence`.lookAndSaySequenceElement(a))
  }
}