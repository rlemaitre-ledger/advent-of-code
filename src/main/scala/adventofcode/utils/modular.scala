package adventofcode.utils.mod

import scala.annotation.tailrec
import scala.annotation.targetName
import scala.math.Integral.Implicits.infixIntegralOps

case class Mod[T](value: T, modulo: T)(using num: Integral[T]):
  @targetName("plus")
  def +(other: Mod[T]): Mod[T] =
    require(modulo == other.modulo, "Cannot add rings with different modulus")
    copy(value = num.plus(value, other.value) % modulo)
  @targetName("minus")
  def -(other: Mod[T]): Mod[T] =
    require(modulo == other.modulo, "Cannot add rings with different modulus")
    copy(value = num.minus(value, other.value) % modulo)
  @targetName("multiply")
  def *(other: Mod[T]): Mod[T] =
    require(modulo == other.modulo, "Cannot multiply rings with different modulus")
    copy(value = num.times(value, other.value) % modulo)
  def one: Mod[T] = Mod(num.one, modulo)

  @targetName("pow")
  def ^(p: Int): Mod[T] =
    require(p >= 0, "p must be zero or greater")
    if (p <= 0) return one
    @tailrec def power(acc: Mod[T], k: Int): Mod[T] = if (k == 1) acc else power(this * acc, k - 1)
    power(this, p)
