import org.joda.time.DateTime

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._


object Timings {
  def main(args: Array[String]) = {
    val input: List[Option[Int]] = (1 to 10000000).map(Some(_)).toList

    val t1 = timeCycling2("collect") {
      input.collect { case Some(x) => x }
    }

    val t2 = timeCycling2("flat map") {
      input.flatMap(_.toList)
    }

    val t3 = timeCycling2("pattern matching") {
      val result = new ListBuffer[Int]
      input.foreach {
        case Some(v) => result += v
        case _ =>
      }
      result.toList
    }

    val t4 = timeCycling2("with list buffer") {
      val result = new ListBuffer[Int]
      input.foreach { x =>
        if (x.nonEmpty) result += x.get
      }
      result.toList
    }

    val t5 = timeCycling2("with list") {
      var result = List.empty[Int]
      input.foreach { x =>
        if (x.nonEmpty) result = x.get :: result
      }
      result
    }

    val t6 = timeCycling2("flatten") {
      input.flatten
    }

    val t7 = timeCycling2("with array list") {
      val result = new java.util.ArrayList[Int](input.count(_.isDefined) + 1)
      input.foreach { x =>
        if (x.nonEmpty) result.add(x.get)
      }
      result.asScala
    }

    val t8 = timeCycling2("with array") {
      val result = new Array[Int](input.count(_.isDefined) + 1)
      var i = 0
      input.foreach { x =>
        if (x.nonEmpty) {
          result.update(i, x.get)
          i += 1
        }
      }
      result.toSeq
    }

    List(t1, t2, t3, t4, t5, t6, t7, t8).zipWithIndex.map { case ((name, (_, time)), i) =>
      s"### t${i + 1}: $time ms ($name)"
    }.foreach(println)

//    ### t1: 79181 ms (collect)
//    ### t2: 24847 ms (flat map)
//    ### t3: 87618 ms (pattern matching)
//    ### t4: 25705 ms (with list buffer)
//    ### t5: 80612 ms (with list)
//    ### t6: 71471 ms (flatten)
//    ### t7: 10554 ms (with array list)
//    ### t8:  6119 ms (with array)

  }



  def timeCycling[T](f: => T): (T, Long) = {

    def timeCyclingRec(i: Int, g: => T) : T = {
      if (i < 10) {
        g
        timeCyclingRec(i + 1, g)
      } else g
    }
    timeCyclingRec(0, f)
    time(f)
  }

  def timeCycling2[T](metric: String)(f: => T): (String, (T, Long)) = {
    (metric, time((1 to 10).map(_ => f).last))
  }

  def time[T](f: => T): (T, Long) = {
    val started = DateTime.now()
    val g = f
    val finished = DateTime.now()
    (g, finished.getMillis - started.getMillis)
  }
}
