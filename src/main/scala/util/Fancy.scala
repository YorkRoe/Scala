package util

object Fancy {

  def main(args: Array[String]): Unit = {
    println(devide(3, 0))
  }

  def devide(x: Int, y: Int): Int = {
    if (y != 0) x / y
    else error("can't divede by zero")
  }

  def error(str: String): Nothing = {
    throw new RuntimeException(str)
  }

}
