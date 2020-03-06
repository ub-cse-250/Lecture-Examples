package cse250.examples.analysis

object BubbleSortMain {
  def main(args: Array[String]): Unit = {
    val vec = for(i<- 1 to 0 by -1; j <- 0 until 5) yield 2*j + i
    println(vec)
    val res = IntBubbleSorter.sort(vec)
    println(res)
    val arr: collection.mutable.ArraySeq[Int] = {for(i<- 1 to 0 by -1; j <- 0 until 5) yield 2*j + i}.toArray
    println(arr)
    println(IntBubbleSorter.sort(arr))
  }
}
