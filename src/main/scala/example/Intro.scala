package example

object Intro {

  val res = (10 - ((4) * 2)) + 1


  val a = 4
  val k1 = (x: Int) => x * 2

  val res2 = (10 -   k1(a)) + 1

  val k2 = (x: Int) => 10 - x


  val res3 = k2(k1(a)) + 1

  val k3 = (x: Int) => x + 1

  val k = k3 compose k2 compose k1

}

object DirectStyle {

  // c * c == a * a + b * b

  def square(x: Int): Int = x * x

  def sum(x: Int, y: Int): Int = x + y

  def pitagoras(a: Int, b: Int): Int = {
    val aa = square(a)
    val bb = square(b)
    sum(aa, bb)
  }

}

object ContinuationPassingStyle {

  def square(x: Int, k: Int => Int): Int = k(x * x)

  def sum(x: Int, y: Int, k: Int => Int): Int = k(x + y)

  def pitagoras(a: Int, b: Int, k: Int => Int): Int =
    square(a, aa =>
      square(b, bb =>
        sum(aa, bb, k)
      )
    )

}