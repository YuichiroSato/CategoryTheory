/**
 * Created by Yuichiro on 2015/02/07.
 */
object Pos {

  trait Poset[A] {

    def getValue: A
    def isDefined(a: A): Boolean
    def ge(a: A, b: A): Boolean
  }

  case class IntPoset() extends Poset[Int] {

    def getValue: Int = ((Math.random() - 0.5) * 2000000).toInt
    def isDefined(a: Int): Boolean = true
    def ge(a: Int, b: Int): Boolean = a <= b
  }

  implicit val intToPoset = IntPoset()

  case class PositiveInt(value: Int)

  case class PositiveIntPoset() extends Poset[PositiveInt] {

    def getValue: PositiveInt = PositiveInt((Math.random() * 1000000).toInt)
    def isDefined(a: PositiveInt): Boolean = 0 <= a.value
    def ge(a: PositiveInt, b: PositiveInt): Boolean = isDefined(a) && isDefined(b) && a.value <= b.value
  }

  implicit val positiveIntToPoset = PositiveIntPoset()

  case class NegativeInt(value: Int)

  case class NegativeIntPoset() extends Poset[NegativeInt] {

    def getValue: NegativeInt = NegativeInt((Math.random() * -1000000).toInt - 1)
    def isDefined(a: NegativeInt): Boolean = a.value < 0
    def ge(a: NegativeInt, b: NegativeInt): Boolean = isDefined(a) && isDefined(b) && a.value <= b.value
  }

  implicit val negativeaIntToPoset = NegativeIntPoset()

  case class BinaryInt(value: Int)

  case class BinaryIntPoset() extends Poset[BinaryInt] {

    def getValue: BinaryInt = if (Math.random() < 0.5) BinaryInt(0) else BinaryInt(1)
    def isDefined(a: BinaryInt): Boolean = a.value == 0 || a.value == 1
    def ge(a: BinaryInt, b: BinaryInt): Boolean = isDefined(a) && isDefined(b) && a.value <= b.value
  }

  case class DoublePoset() extends Poset[Double] {

    def getValue: Double = (Math.random() - 0.5) * 20000000
    def isDefined(a: Double): Boolean = true
    def ge(a: Double, b: Double): Boolean = isDefined(a) && isDefined(b) && a <= b
  }

  implicit val doubleToPoset = DoublePoset()

  case class PositiveDouble(value: Double)

  case class PositiveDoublePoset() extends Poset[PositiveDouble] {

    def getValue: PositiveDouble = PositiveDouble(Math.random() * 10000000)
    def isDefined(a: PositiveDouble): Boolean = 0 <= a.value
    def ge(a: PositiveDouble, b: PositiveDouble): Boolean = isDefined(a) && isDefined(b) && a.value <= b.value
  }

  implicit val positiveDoubleToPoset = PositiveDoublePoset()

  case class NegativeDouble(value: Double)

  case class NegativeDoublePoset() extends Poset[NegativeDouble] {

    def getValue: NegativeDouble = NegativeDouble(Math.random() * -10000000)
    def isDefined(a: NegativeDouble): Boolean = a.value < 0
    def ge(a: NegativeDouble, b: NegativeDouble): Boolean = isDefined(a) && isDefined(b) && a.value <= b.value
  }

  implicit val negativeDoubleToPoset = NegativeDoublePoset()

  case class StringPoset() extends Poset[String] {

    def getValue: String = {
      def random: String = (Math.random() * 26 + 97).toChar.toString
      val n = (Math.random() * 100).toInt
      List.fill(n)(random).fold("")(_ + _)
    }
    def isDefined(a: String): Boolean = true
    def ge(a: String, b: String): Boolean = isDefined(a) && isDefined(b) && a <= b
  }

  implicit val stringToPoset = StringPoset()

  case class AString(value: String)

  case class AStringPoset() extends Poset[AString] {

    def getValue: AString = {
      val n = (Math.random() * 100).toInt
      AString(List.fill(n)("a").fold("")(_ + _))
    }
    def isDefined(a: AString): Boolean = a.value.count(_ != 'a') == 0
    def ge(a: AString, b: AString): Boolean = isDefined(a) && isDefined(b) && a.value <= b.value
  }

  implicit val aStringToPoset = AStringPoset()

  def isMonotone[A, B](f: A => B)(implicit p: Poset[A], q: Poset[B]): Boolean = {
    var result = true
    for (_ <- 0 to 10000) {
      val v1 = p.getValue
      val v2 = p.getValue

      if (p.ge(v1, v2))
        result &&= q.ge(f(v1), f(v2))
      else
        result &&= q.ge(f(v2), f(v1))
    }
    result
  }

  def counterExample[A, B](f: A => B)(implicit p: Poset[A], q: Poset[B]): Unit = {
    for (_ <- 0 to 10000) {
      val v1 = p.getValue
      val v2 = p.getValue

      val f1 = f(v1)
      val f2 = f(v2)

      if (p.ge(v1, v2) && q.ge(f2, f1) && !q.ge(f1, f2)) {
        println(v1 + " < " + v2 + ", f(" + v2 + ") = " + f2 + " < f(" + v1 + ") = " + f1)
        return
      }
      else if (p.ge(v2, v1) && q.ge(f1, f2) && !q.ge(f2, f1)) {
        println(v2 + " < " + v1 + ", f(" + v1 + ") = " + f1 + " < f(" + v2 + ") = " + f2)
        return
      }
    }
  }

  def equal[A, B](f: A => B, g: A => B)(implicit p: Poset[A]): Boolean = {
    var result = true
    for (_ <- 0 to 10000) {
      val v = p.getValue
      result &&= f(v) == g(v)
    }
    result
  }

  val idInt = (i: Int) => i

  val idPositiveInt = (p: PositiveInt) => p

  val idNegativeInt = (n: NegativeInt) => n

  val idBinaryInt = (b: BinaryInt) => b

  val idDouble = (d: Double) => d

  val idPositiveDouble = (p: PositiveDouble) => p

  val idNegativeDouble = (n: NegativeDouble) => n

  val idString = (s: String) => s

  val idAString = (a: AString) => a

  val add1 = (a: Int) =>  a + 1

  val mod2 = (i: Int) => BinaryInt(i % 2)

  val isPositive = (i: Int) => if (i < 0) BinaryInt(0) else BinaryInt(1)

  val binaryToInt = (b: BinaryInt) => b.value

  val minMax = (b: BinaryInt) => if (b.value == 0) -1000000 else 1000000

  val intToPositiveInt = (i: Int) => if (i <= 0) PositiveInt(0) else PositiveInt(i)

  val intToSqrt = (i: Int) => if (i <= 0) 0.0 else Math.sqrt(i)

  val inverse = (p: PositiveInt) => NegativeInt(-p.value)

  val intToDouble = (i: Int) => i.toDouble

  val sqrt = (p: PositiveInt) => Math.sqrt(p.value)

  val roundOff = (d: Double) => if ((d % 1) < 0.5) d.toInt else d.toInt + 1

  val round = (d: Double) => d.toInt

  val length = (str: String) => str.length

  val duplicate = (str: String) => str + str

  val Alength = (astr: AString) => astr.value.length

  val aStringToString = (astr: AString) => astr.value

}
