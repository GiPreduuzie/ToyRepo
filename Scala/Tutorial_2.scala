// что делает эта функция?  

def function_1(a: Array[Int]): String = {
    var index = 2
    var result = ""

    if (a.length != 0) {
      result = a(0).toString

      while (index < a.length) {
        result = result + ", " + a(index)
        index = index + 2
      }
    }

    result
  }
