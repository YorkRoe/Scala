package layout

import layout.Element.elem

object Main {
  def main(args: Array[String]): Unit = {
    val ae: Element = elem(Array("one", "two"))
    println(ae.height)
    println(ae.width)
  }
}
