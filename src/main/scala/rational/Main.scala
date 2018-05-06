package rational

object Main {
  def main(args: Array[String]): Unit = {
    implicit def intToRational(n: Int) = new Rational(n)

    println(2 * new Rational(3, 2))

  }
}
