package traits

object Philosopher {
  def main(args: Array[String]): Unit = {
    val Frog = new Frog()
    println(Frog.philosophize())
  }
}

class Animal

trait HasLegs

trait Philosophical {
  def philosophize(): Unit = {
    println("I consume memory, therefore I am!")
  }
}

class Frog extends Animal with Philosophical with HasLegs {
  override def philosophize(): Unit = {
    println("It ain't easy being " + toString + "!")
  }

  override def toString: String = "green"
}




