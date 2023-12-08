package adventofcode.utils

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps

@tailrec
def gcd[T](a: T, b: T)(using num: Integral[T]): T = if (b == 0) a.abs else gcd(b, a % b)
def lcm[T](list: IterableOnce[T])(using num: Integral[T]): T = list.iterator.foldLeft(num.one)((a, b) => (a / gcd(a, b)) * b)
 
