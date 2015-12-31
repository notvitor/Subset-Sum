import scala.collection.mutable.ListBuffer

object Main extends App {

  //Using the given example.
  val numbers = Array(1, 2, 3, 4, 6)
  val sum = 6
  time(recursiveSubset(numbers, sum, accum = new ListBuffer[Int]()))

  //With negative numbers.
  val numbers1 = Array(-1, 1, 2, 3, 4, 6)
  val sum1 = 6
  //time(recursiveSubset(numbers1, sum1, accum = new ListBuffer[Int]()))

  //Different example with distinct integers unordered from 1 to 100.
  val numbers2 = Array(47, 66, 94, 11, 69, 100, 85, 84, 93, 15
    , 2, 64, 62, 89, 22, 45, 72, 70, 88, 21, 35, 95, 78, 40, 43, 14
    , 32, 87, 27, 71, 8, 17, 75, 63, 29, 90, 54, 38, 82, 12, 77, 57
    , 50, 34, 97, 59, 24, 53, 26, 42, 99, 25, 51, 48, 80, 60, 81, 86
    , 1, 83, 5, 56, 74, 9, 7, 52, 68, 4, 61, 18, 91, 33, 10, 28, 92, 76
    , 96, 58, 46, 37, 3, 31, 55, 49, 6, 30, 65, 98, 23, 16, 73, 13, 67
    , 41, 44, 79, 39, 36, 19, 20)
  val sum2 = 100
  //time(recursiveSubset(numbers2, sum2, accum = new ListBuffer[Int]()))

  /**
   * Implementing using recursion. Idea of DFS-Depth-first search. Duplicated values are not treated.
   * The 'time complexity' is increased exponentially as the combinations are performed.
   * @param list list of numbers
   * @param sum aimed value
   * @param accum accumulator to store the values aiming the sum
   */
  def recursiveSubset(list: Array[Int], sum: Int, index: Int = 0, current: Int = 0, accum: ListBuffer[Int]): Unit = {
    if (list.length < index || current > sum) return
    for (i <- index until list.length) {
      if (current + list(i) == sum)
        formatOutput(accum.toList :+ list(i))

      recursiveSubset(list, sum, i + 1, current + list(i), accum += list(i))
      accum -= list(i)
    }
  }

  //Time the function
  def time[R](f: => R): R = {
    val t1 = System.nanoTime()
    val result = f
    println(s"time: ${(System.nanoTime-t1)/1e6}ms")
    result
  }

  //Requested printing format.
  def formatOutput[Int](list: List[Int]): Unit = list match {
    case x :: xs => println(xs.foldLeft("(" + x)(_ + ", " + _) + ")")
    case _ => println("()")
  }


  // (2) Alternative solution
  // Dynamic programming approach(DP). This implementation does not combine all the values as the implementation above.
  // Therefore, it only holds the final sum of the combinations and not the subsets used for calculation.
  def dynamicProgSubset(list: Array[Int], sum: Int): Unit = {

    val store = Array.ofDim[Boolean](sum + 1,numbers.length + 1)
    (0 to list.length).foreach(store(0)(_) = true)
    (1 to sum).foreach(store(_)(0) = false)

    for (i <- 1 to sum) {
      for (j <- 1 to list.length) {
        store(i)(j) = store(i)(j - 1)
        if (i >= list(j - 1))
          store(i)(j) = store(i)(j) || store(i - list(j - 1))(j - 1)
      }
    }
    println(s"Has a subset? A: ${store(sum)(list.length)}")
  }
}
