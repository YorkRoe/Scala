package util

object mutableObjects {

  def main(args: Array[String]): Unit = {
    val time = new Time
  }
}

class Time {
  var m = 0
  private[this] var h: Int = 12
}

class BankAccount {
  private var bal: Int = 0

  def balance: Int = bal

  def withdraw(amount: Int): Boolean = {
    if (amount > bal) false
    else {
      bal -= amount
      true
    }
  }
}
