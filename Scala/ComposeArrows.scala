import cats.arrow.Arrow
import cats.data.Kleisli
import cats.implicits._

import scala.language.higherKinds
import scala.util.Try

// Задача. Даны:
// fab: A => F[B]
// fbc: B => F[C]
// fccd: (C, C) => F[D]
// Нужно собрать функцию (А, A) => F[D]

object ArrowsExc {
  // без type project приходится использовать псевдоним с фиксированным F = Option
  type KleisliOpt[A, B] = Kleisli[Option, A, B]

  def kleisli[A, B](fab: A => Option[B]) : KleisliOpt[A, B] = Kleisli(fab)

  def main(args: Array[String]): Unit = {
    val kab = kleisli(fab)
    val kbc = kleisli(fbc)
    val kccd = kleisli(fccd)

    val kac = kab >>> kbc
    val f = (kac split kac) >>> kccd

    println(f("8", "16"))

    // или через вызов функции (чтобы показать сигнатуру)
    println(custom(kab, kbc, kccd).apply("9", "16"))

    // будет напечатано
    // Some(2.8284271247461903 + 4.0)
    // Some(3.0 + 4.0)
  }

  def custom[F[_, _]: Arrow, A, B, C, D](fab: F[A, B], fbc: F[B, C], fcd: F[(C, C), D]): F[(A, A), D] = {
    val fac = fab >>> fbc
    (fac split fac) >>> fcd
  }

  def fab(v : String): Option[Int] = Try(v.toInt).toOption

  def fbc(v : Int): Option[Double] = if (v >= 0) Some(Math.sqrt(v)) else None

  def fccd(v : (Double, Double)): Option[String] = Some(v._1 + " + " + v._2)

}
