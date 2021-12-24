package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework {
  
  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    
    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
    // Optional task
    def toInt: Int
    
    // Optional task
    def fromInt(i: Int): Nat = {

      if (i < 0)
        throw new Exception("Can not convert from negative number")

      @tailrec
      def inner(i: Int, to_return: Nat): Nat = i match {
        case 0 => to_return
        case p => inner(i - 1, Succ(to_return))
      }

      inner(i, Zero)
    }
  
    override def toString: String = s"Succ($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat {

    // zero is zero
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")
    
    // zero + anything = anything
    infix def +(that: Nat): Nat = that
    
    // we can not subtract from zero in the field of natural numbers
    infix def -(that: Nat): Nat = that match {
      case _:Zero => Zero
      case _ => throw new Exception("Nat can not be negative (only Zero or Succ)")
    }
    
    // this class representes zero
    def toInt: Int = 0

    override def toString: String = "Zero"

    // since we represent number in this class
    // we can only compare it with number types, Zero and Succ
    override def equals(obj: Any): Boolean = obj match {
      // zero = zero
      case _:Zero => true

      // this is also case for Succ
      case _ => false
    }
  }

  class Succ(n: Nat) extends Nat {

    // Succ instance represents positive number
    def isZero: Boolean = false
    def predecessor: Nat = n
    
    infix def +(that: Nat): Nat = that match {
      case z: Zero => this
      case _       => Succ(this) + that.predecessor
    }
    
    infix def -(that: Nat): Nat = that match {
      // anything - zero = anything
      case _:Zero => this

      case _ => this.predecessor - that.predecessor
    }
    
    // recursive computing
    def toInt: Int = {
      
      @tailrec
      def inner(to_return: Int, predecessor: Nat): Int = predecessor match {
        case p: Zero => to_return + 1
        case p: Succ => inner(to_return + 1, p.predecessor)
      }

      inner(0, this.predecessor)
    }

    // since we represent number in this class
    // we can only compare it with number types, Zero and Succ
    override def equals(obj: Any): Boolean = obj match {
      // recursive comparation
      case p: Succ => this.predecessor == p.predecessor

      // this is also case for Zero
      case _ => false
    }
  }

  @main
  def main(): Unit = {
    val num1 = Succ(Succ(Succ(Zero)))

    print(num1 + Succ(Succ(Zero)))
  }
}
