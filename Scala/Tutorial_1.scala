// вот так мы оставляем комментарии к коду

// объявим несколько постоянных разных типов
val a : Int = 1 // целое число в ячейке 32 бита
val b : Long = 123L // целове число в ячейке 64 бита, буква L в конце литерала как раз даёт понять компилятору, что речь о Long, а не об Int
val c : Float = 1.1F // число с плавающей точкой, буква F в конце литерала даёт понять компилятору, что речь о Float, а не о Double, 32 бита
val d : Double = 1.1 // число с плавающей точкой удвоенной точности, 64 бита

// распечатаем значения с указанием типа
println(s"$a of type ${a.getClass.getCanonicalName}")
println(s"$b of type ${b.getClass.getCanonicalName}")
println(s"$c of type ${c.getClass.getCanonicalName}")
println(s"$d of type ${d.getClass.getCanonicalName}")

// объявим переменную; теперь ключевое слово var (variable, переменная), а не val (value, значение)
// постоянные нельзя менять, переменные - можно
var intVariable = 0
// прибавим к переменной единицу
intVariable = intVariable + 1
println(intVariable)

// объявим функцию
def plus(a: Int, b: Int) = a + b

// вызовем функцию
val plusResult = plus(5, 9)
println(plusResult)

// функции можно определять и в несколько строк, для этого используют фигурные скобки
def minus(a: Int, b: Int) = {
  println (s"arguments of `minus` function: $a, $b")
  a - b
}

// вызываются они ровно также
val minusResult = minus(5, 9)
println(minusResult)

// объявим целочисленный массив на 10 ячеек по 32 бита.
// Вместо литерала `10` можно было использовать целочисленую переменную или постоянную
val array = new Array[Int](10)
// первый элемент массива; традиционно считаем с 0
var first = array(0)
// последний элемент массива
var last = array(9)
println("если попытататься распечатать массив, будет показаны не его значения, а ссылка на объект: " + array)
println("распечатать массив можно так: " + array.mkString(", "))
println("по умолчанию целочисленный массив заполняется нулями, первый элемент: " + first)
println("последний элемент: " + last)
// мы объявили массив как постоянную (val), и поэтому значение array, т.е. ссылку на массив, нельзя менять
// однако элементы массива менять можно
array(1) = 1
array(5) = 5
array(8) = 8
println(array.mkString(", "))

// воспользуемся циклом while, чтобы заполнить массив значениями
var index = 0
while (index < 10) {
  array(index) = index*index
  index = index + 1
}

println(array.mkString(", "))

// аналогично, но с помощью цикла for
for (i <- 0 until 10) {
  array(i) = i*i
}

println(array.mkString(", "))


