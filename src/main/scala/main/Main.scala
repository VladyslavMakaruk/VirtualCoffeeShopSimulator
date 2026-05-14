package main
import inputParser._

// scala Test + scala Check
object Main {
  def main(args: Array[String]): Unit = {
  }
  
  def forComprehensionTest(): Unit = {
    val li = List(1,2,3,4)
    val ls = List('a','b','c','d')
    val lc = List("black","white")
    println({
      for {
        x <- li
        y <- ls
      } yield s"${x}-${y}"
    })
    val listOfCombinations = li.flatMap(x => ls.map(y => s"$x-$y" ) )
    val allPossibleCombinations = li.flatMap(i => ls.flatMap(s => lc.map(c => s"$s-$c-$i")))
  }
}