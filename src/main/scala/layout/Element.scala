package layout

import layout.Element.elem

abstract class Element {
  def contents: Array[String]
  def height: Int = contents.length
  def width: Int = if (contents.isEmpty) 0 else contents.head.length

  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }

  def beside(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(
      for (
        (line1, line2) <- this1.contents zip that1.contents
      ) yield line1 + line2
    )
  }

  def widen(w: Int): Element = {
    if (w <= width) this
    else {
      val left = elem(' ', (w - width) / 2, height)
      val right = elem(' ', w - width - left.width, height)
      left beside this beside right
    }
  }

  def heighten(h: Int): Element = {
    if (h <= height) this
    else {
      val top = elem(' ', width, (h - height) / 2)
      val bot = elem(' ', width, h - height - top.height)
      top above this above bot
    }
  }

  override def toString: String = contents mkString "\n"
}

object Element {
  def elem(contents: Array[String]): Element = new ArrayElement(contents)
  def elem(s: String): Element = new LineElement(s)
  def elem(ch: Char, height: Int, width: Int): Element = new UniformElement(ch, height, width)

  private class ArrayElement(val contents: Array[String]) extends Element {}
  private class LineElement(s: String) extends Element {
    val contents = Array(s)

    override def height: Int = 1

    override def width: Int = s.length
  }
  private class UniformElement(ch: Char, override val height: Int, override val width: Int) extends Element {
    private val raw = ch.toString * width

    def contents: Array[String] = Array.fill(height)(raw)
  }

}




