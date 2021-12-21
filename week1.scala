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

object Homework {

  object `Boolean Operators` {

    /*
    not function for Boolean argument

    not(false) => true
    not(true) => false

    There are some equivalent realizations:

    Without else statement:
    def not(b: Boolean) = {
      if (b) false
      true
    }    

    With match statement:
    def not(b: Boolean) = b match {
      case true => false
      case false => true
    }    
    */
    def not(b: Boolean) = {
      // not (false) = true
      if (b) false
      // not(false) = true
      else true
    }

    /*
    and function for two Boolean arguments
    Return true if both arguments are true else false

    There are some alternative realizations:

    With additional if statement:
    def and(left: Boolean, right: Boolean): Boolean = {
      if (left)
        if (right)
          true
        else
          false
      else
        false
    }    

    With match statement:
    def and(left: Boolean, right: Boolean): Boolean = left match{
      case true => right
      case false => false
    }
    */
    def and(left: Boolean, right: Boolean): Boolean = {
      if (left)
        // and(true, right) = right
        right
      else
        // and(false, right) = false
        false
    }

    /*
    or function for two Boolean arguments
    
    or(false, true) = true
    or(true, false) = true
    or(true, true) = true
    or(false, false) = false

    There is equivalent realization using match statement:
    def or(left: Boolean, right: Boolean): Boolean = left match {
      case true => true
      case false => right
    }
    */
    def or(left: Boolean, right: Boolean): Boolean = {
      if (left)
        // or(true, right) = true
        true
      else
        // or(false, rigth) = right
        right
    }
  }

  /*
  Note. For following tasks inner tail recursion 
  functions were used. It's made remove the possibility for the user 
  to interact with the acumulator variable.
  */
  object `Fermat Numbers` {

    /*
    Function with two Long arguments;
    Returns product of numbers;

    Note. In fact, fo current task non-negative numbers
    because Fermat numbers task doesn't include non-negative numbers.

    Example:
    >>> multiplication(5, 4)
    20
    >>> multiplication(-3, 6)
    -18
    */
    val multiplication: (Long, Long) => Long = (x, y) => {
      
      /*
      Inner function, that realizes tail recursion.
      acumulator stores current product of two numbers.
      */
      @tailrec
      def mult(x: Long, y: Long, acumulator: Long = 0): Long = y match {
        // in this case acumulator stores x * y
        case 0 => acumulator

        // change signs of multipliers 
        case _ if (y < 0) => mult(-x, -y, acumulator)

        // in this case acumulator stores x * k (k < y)
        case _ => mult(x, y + (-1), acumulator + x)
      }
      
      // compute product using tail recusrion
      mult(x, y)
    }

    /*
    Functions for two Long arguments.
    Implements (x, y) -> x^y.

    Because of Long returning type, 
    y (power) is non-negative numbers.

    Example:
    >>> power(3, 6)
    729
    >>> power(2, 6)
    64
    */
    val power: (Long, Long) => Long = (x, y) => {
      
      @tailrec
      def pow(x: Long, y: Long, acumulator: Long = 1): Long = y match {
        // in this case acumulator stores x^y
        case 0 => acumulator

        case _ if y < 0 => throw new Exception("Power has to be non-negative number.")

        // in this case acumulator stores x^k (k < y)
        case _ => pow(x, y + (-1), multiplication(acumulator, x))
      }
      
      pow(x, y)
    }

    /*
    for Fermat numbers generation I gonna
    use reccurent formula:
    Fn = (Fn-1 - 1)^2 + 1, n => 0, F0 = 3

    This is function for one Int argument.
    Returns nth Fermat number.

    Example:
    >>> fermatNumber(0)
    3
    >>> fermatNumber(3)
    257

    Question: what about Option[Long] returning type?
    if n < 1 we would be able to return Option, and
    then use getOrElse method.
    */
    val fermatNumber: Int => Long = (n) => {
      
      @tailrec
      // F0 = 3, so default value of acumulator is 3
      def computeNumber(n: Int, acumulator: Long = 3): Long = n match {
        // in this case acumulator stores nth Fermat number
        case 0 => acumulator
        /* Note. Here we can use function power2(x: Long), which is implemented as follows:
        def power2(x: Long): Long = multiplication(x, x)
        

        Case statement in such way will look like:
        case _ => computeNumber(n + (-1), power2(acumulator + (-1)) + 1)
        */

        case _ if n < 0 => throw new Exception("N has to be non-negative number.")

        // in this case acumulator stores kth Fermat number (k < n)
        case _ => computeNumber(n + (-1), power(acumulator + (-1), 2) + 1)
      }
      
      computeNumber(n)
    }
  }


  
  object `Look-and-say Sequence` {
    val lookAndSay: Int => Long = (n) => {

      // Converst number string representation
      // to look-and-say sequence element
      def generateNumber(number: String): String = {

        // generate next number from given
        // using tail recursion
        @tailrec
        def generate(
            number: String,
            repeat: Char,
            i: Int = 0,
            times: Int = 1,
            acumulator: String = ""): String = number charAt i match {
          // end of the string
          // write last digit and its times and stop
          case ' ' => acumulator + times.toString + repeat

          // write digit and its times (111 -> 31)
          // also, go to next symbol by i + 1
          // set times to 1
          case s if (s != repeat) =>
            generate(number, s, i + 1, 1, acumulator + times.toString + repeat)

          // increment counter of current digit
          // also, go to next symbol by i + 1
          case _ => generate(number, repeat, i + 1, times + 1, acumulator)
        }

        generate(number.tail + ' ', number.head)
      }

      @tailrec
      def inner(n: Int, acumulator: String = "1"): String = n match {

        // in this case acumulator stores nth
        // element of look-and-say sequence
        case 1 => acumulator

        // generate next number
        case _ => inner(n - 1, generateNumber(acumulator))
      }

      // because of signature
      // n is non-negative number
      // so inner(n) can be converted
      // into Long type
      inner(n).toLong
    }
  } 

  object `Kolakoski sequence` {
    val kolakoskiSequence: Int => Int = (num) =>
      num match {

        // values which can not
        // be generated
        case 1 => 1
        case 2 => 2
        case 3 => 2
        
        // generate seq
        // using tail rec function
        case _ => {

          // seqLen is used to
          // optimize function
          // standard solve is to
          // iterate i from 2 to n - 1
          // here sequence generation stops
          // when its len >= n
          @tailrec
          def inner(maxI: Int, 
                    seq: List[Int] = List(1, 2, 2), 
                    i: Int = 2,
                    seqLen:Int = 3): Int = seqLen match {
            
            // generate sequence
            case s if (s < maxI) =>
            
              // new value for sequence
              val toAppend: Int = 1 + i % 2;
            
              // add value one time
              if (seq(i) == 1)
                inner(maxI, 
                      seq ++ List(toAppend), 
                      i + 1, 
                      seqLen + 1)
            
              // add value twice
              else
                inner(maxI, 
                      seq ++ List(toAppend, toAppend), 
                      i + 1, 
                      seqLen + 2)
            
            // len of generated sequence
            // is equal to or greater than given number
            case _ => seq(maxI - 1)
          }

          inner(num)
        }
     }
  }
}
