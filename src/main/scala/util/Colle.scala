package util

import scala.collection.mutable

object Colle {

  def main(args: Array[String]): Unit = {
    val list = List(1, 3, 2, 6, -1, -2)

    val test: List[Nothing] = List()
    val test2: List[Nothing] = Nil
    println(test == test2)

    val nums = 1 :: 2 :: 3 :: Nil

    val head: Any = list.head
    val tail: List[Any] = list.tail //all elements except head
    val empty: Boolean = list.isEmpty

    val fruits: List[String] = List("apples", "oranges", "pears")
    val a :: b :: rest = fruits
    val List(d, e, f) = fruits

    val flatten: List[Int] = List(List(1,2),List(3), List(), Nil, List(4,5)).flatten
    val charFruits = fruits.map(_.toCharArray).flatten
    println(flatten.mkString("["," ","]"))
    println(charFruits.mkString(" "))

    val zip = charFruits.indices zip charFruits
    println(zip.mkString(" "))
    println(charFruits.zipWithIndex.mkString(" "))
    println(zip.unzip._1.mkString(" "))

    def fun = (x: Int, y: Int) => x < y

    msort(fun)(list)

    println(List.range(3,5))

    //filter(p) && filter(!p)
    val part: (List[Int], List[Int]) = List(1,2,3).partition(_%2 == 0)

    //takeWhile && dropWhile
    val span: (List[Int], List[Int]) = list.span(_%2>0)

    hasZeroRow(List(List(1, 0), List(0, 0)))

    val n = List(4, 2, 9, 3, 1)
    val sum = (0 /: n) (_ + _)
    val sum1 = (n :\ 0) (_ + _)
    println(sum == sum1)

    val fill: List[List[Int]] = List.fill(2,3)(0)
    println(List.tabulate(4)(n=>n*n))

    (List(2,3,4), List(0,4)).zipped.map(_*_)

  }

  def longestWord(words: Array[String]) ={
    var word = words(0)
    var idx = 0
    for( i <- 1 until words.length-1){
      if(words(i).length > word.length)
        word = words(i)
      idx = i
    }
    (word, idx)
  }

  def countWords(text :String)={
    val counts = mutable.Map.empty[String, Int]
    for(rawWord <- text.split("[ ,!.+")){
      var word = rawWord.toLowerCase
      var oldCount = if (counts.contains(word)) counts(word)
                      else 0
      counts += (word -> (oldCount+1))
    }
    counts
  }

  def reverseLeft[T](xs: List[T]): List[T] = {
    (List[T]() /: xs) ((ys, y) => y :: ys)
  }


  def foldL(xs:List[Int]):Int={
    (0 /: xs) (_ + _)
  }

  def hasZeroRow(m: List[List[Int]]):Boolean={
    m.exists(row => row.forall(_==0))
  }

  def append[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case x :: xs1 => x :: append(xs1, ys)
  }

  def rev[T](xs: List[T]): List[T] = xs match {
    case List() => xs
    case x :: xs1 => rev(xs1) ::: List(x)
  }

  //MERGE SORT
  def msort[T](less: (T, T) => Boolean)(xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs splitAt n
      merge(msort(less)(ys), msort(less)(zs))
    }
  }

  //INSERTION SORT
  def isort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) Nil
    else insert(xs.head, isort(xs.tail))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = {
    if (xs.isEmpty || x <= xs.head) x :: xs
    else xs.head :: insert(x, xs.tail)
  }

  def isortPattern(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case x :: xs1 => insertPattern(x, xs1)
  }

  def insertPattern(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x <= y) x :: xs
    else y :: insert(x, ys)
  }

}
