import java.util

import org.joda.time.DateTime

import scala.collection.GenTraversableOnce
import scala.collection.mutable.ListBuffer


object Timings {
  def main(args: Array[String]) = {
    val input : List[Option[Int]] = (1 to 10000000).map(Some(_)).toList

    val t1 = timeCycling2 {
      input.collect { case Some(x) => x }
    }

    val t2 = timeCycling2 {
      input.flatMap(_.toList)
    }

    val t3 = timeCycling2 {
      val result = new ListBuffer[Int]
      input.foreach {
        case Some(v) => result += v
        case _ =>
      }
      result.toList
    }

    val t4 = timeCycling2 {
      val result = new ListBuffer[Int]
      input.foreach { x =>
        if (x.nonEmpty) result += x.get
      }
      result.toList
    }

    val t5 = timeCycling2 {
      var result = List.empty[Int]
      input.foreach { x =>
        if (x.nonEmpty) result = x.get :: result
      }
      result
    }

    val t6 = timeCycling2 {
      input.flatten
    }


    val t7 = timeCycling2 {
      val result = new java.util.ArrayList[Int](input.count(_.isDefined) + 1)
      input.foreach { x =>
        if (x.nonEmpty) result.add(x.get)
      }
      result
    }

    import ToMySeqExtentions._

    val t8 = timeCycling2 {
      input.collect2(_.isDefined, _.get)
    }

    println("### t1: " + t1._2)
    println("### t2: " + t2._2)
    println("### t3: " + t3._2)
    println("### t4: " + t4._2)
    println("### t5: " + t5._2)
    println("### t6: " + t6._2)
    println("### t7: " + t7._2)

//    ### t1: 73 538
//    ### t2: 28 657
//    ### t3: 94 164
//    ### t4: 27 395
//    ### t5: 80 442
//    ### t6: 78 928
//    ### t7: 10 668
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

  def timeCycling2[T](f: => T): (T, Long) = {
    time((1 to 10).map(_ => f).last)
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
