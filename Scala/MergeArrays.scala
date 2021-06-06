object MergeArrays {

  def main(args: Array[String]): Unit = {
    val result = mergeArrays(Array(2, 3, 4, 5), Array(4, 5, 6, 7))
    println(result.mkString(", "))
  }

  def mergeArrays(a: Array[Int], b: Array[Int]): Array[Int] = {
    val c = new Array[Int](a.length + b.length)
    var i_a = 0
    var i_b = 0
    var i_c = 0
    while (i_a < a.length && i_b < b.length)
      if (a(i_a) <= b(i_b)) {
        c(i_c) = a(i_a)
        i_a = i_a + 1
        i_c = i_c + 1
      }
      else {
        c(i_c) = b(i_b)
        i_b = i_b + 1
        i_c = i_c + 1
      }
    c
  }

}

