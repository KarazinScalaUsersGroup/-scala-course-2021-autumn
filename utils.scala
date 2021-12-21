package karazin.scala.users.group.week2.homework

import scala.math.BigDecimal

object utils:

  def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

  // to format some really long doubles 
  // due to constraints of long doubles arithmetic operations
  def doubleFormat(d: Double): Double =
    BigDecimal(d).setScale(10, BigDecimal.RoundingMode.HALF_UP).toDouble