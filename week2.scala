package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.math.{abs, signum}

object Homework {

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int) {
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    
    @targetName("addition")
    infix def +(that: Rational): Rational = 
      // add by formula: a/b + c/d = (a * d + b * c) / (b * d)
      Rational(this.numer * that.denom + that.numer * this.denom,
               this.denom * that.denom)

    @targetName("negation")
    infix def unary_- : Rational = 
      // - (x / y) = -x / y
      Rational(-this.numer, this.denom)

    
    @targetName("substraction")
    infix def -(that: Rational): Rational = 
      // a / b - c / d = (a * d - b * c) / (b * d)
      Rational(this.numer * that.denom - that.numer * this.denom,
               this.denom * that.denom)

    
    @targetName("multiplication")
    infix def *(that: Rational): Rational = 
      // (a / b) * (c / d) = (a * c) / (b * d)
      Rational(this.numer * that.numer,
               this.denom * that.denom)


    @targetName("division")
    infix def /(that: Rational): Rational = 
      // (a / b) / (c / d) = (a * d) / (b * c)
      // it's important, that denominator has to be positive
      Rational(signum(that.numer) * this.numer * that.denom,
               signum(that.numer) * that.numer * this.denom)

    
    override def equals(other: Any): Boolean = other match {
      // compare with number
      case p @ (_:Int | _:Float | _:Double) => (this.numer / this.denom).equals(p)

      // compare with sequence with two elements of type Int
      // Rational(1, 2) = List(1, 2)
      case s: Seq[Int] if (s.size == 2) => (this.numer.equals(s(0)) && this.denom.equals(s(1)))

      // there are no more types to be compared
      case _ => false
    }
    
    // LaTeX representation of rational number
    def toLatex: String = s"\\frac{${{this.numer}}}{${this.denom}}"

    // string representation of number
    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    // used to reduce fraction
    private lazy val g = gcd(abs(x), y)
  }
}