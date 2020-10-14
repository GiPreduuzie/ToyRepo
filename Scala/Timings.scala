import org.joda.time.DateTime

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._


object Timings {
  def main(args: Array[String]) = {
    val input : List[Option[Int]] = (1 to 10000000).map(Some(_)).toList

    val t1 = timeCycling2 ("collect") {
      input.collect { case Some(x) => x }
    }

    val t2 = timeCycling2 ("flat map") {
      input.flatMap(_.toList)
    }

    val t3 = timeCycling2 ("pattern matching") {
      val result = new ListBuffer[Int]
      input.foreach {
        case Some(v) => result += v
        case _ =>
      }
      result.toList
    }

    val t4 = timeCycling2 ("with list buffer") {
      val result = new ListBuffer[Int]
      input.foreach { x =>
        if (x.nonEmpty) result += x.get
      }
      result.toList
    }

    val t5 = timeCycling2 ("with list recursion") {
      var result = List.empty[Int]
      input.foreach { x =>
        if (x.nonEmpty) result = x.get :: result
      }
      result
    }

    val t6 = timeCycling2 ("flatten") {
      input.flatten
    }

    val t7 = timeCycling2 ("with array list") {
      val result = new java.util.ArrayList[Int](input.count(_.isDefined) + 1)
      input.foreach { x =>
        if (x.nonEmpty) result.add(x.get)
      }
      result.asScala
    }

    val t8 = timeCycling2 ("with array") {
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

    List(t1, t2, t3, t4, t5, t6, t7, t8).foreach { case (name, (_, time)) =>
      println(s"### t1: $time ms ($name)")
    }

//    ### t1: 74266 ms (collect)
//    ### t1: 28899 ms (flat map)
//    ### t1: 92293 ms (pattern matching)
//    ### t1: 29327 ms (with list buffer)
//    ### t1: 80015 ms (with list recursion)
//    ### t1: 73811 ms (flatten)
//    ### t1: 10598 ms (with array list)
//    ### t1:  6316 ms (with array)

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

object ToMySeqExtentions {
  implicit def toMySeqExtentions[T](xs: Seq[T]): MySeqExtentions[T] = new MySeqExtentions(xs)
}

class MySeqExtentions[A](xs: Seq[A]) {
  import scala.collection.JavaConversions._

  def collect2[B](p: A => Boolean, f: A => B) = {
    val result = new java.util.ArrayList[B](xs.count(x => p(x)) + 1)
    xs.foreach { x => if (p(x)) result.add(f(x)) }
    result.toList
  }
}
