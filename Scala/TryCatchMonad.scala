import scala.util.{Failure, Success, Try}

object TryCatchMonad {

  def main(args: Array[String]) = {

    val res = for {
      i <- Try(a()).mapException(ex => "a: " + ex.getMessage)
      j <- Try(b()).mapException(ex => "b: " + ex.getMessage)
      k <- Try(c()).mapException(ex => "c: " + ex.getMessage)
    } yield {
      i * j * k
    }

    println(res.resolve(_.toString, identity))
  }

  def a(): Int = {
    1
  }

  def b(): Int = {
    ???
  }

  def c(): Int = {
    3
  }

  implicit class TryCatchExt[T](tryExp: Try[T]) {

    def mapException[U](f: Throwable => U): TryCatch[T, U] = {
      tryExp match {
        case Failure(exception) => TryError(f(exception))
        case Success(value) => TryValue(value)
      }
    }

  }

  sealed trait TryCatch[T, U] {
    def map[R](f: T => R): TryCatch[R, U]

    def flatMap[R](f: T => TryCatch[R, U]): TryCatch[R, U]

    def resolve[R](onValue: T => R, onError: U => R): R
  }

  case class TryValue[T, U](value: T) extends TryCatch[T, U] {
    override def map[R](f: T => R): TryCatch[R, U] = copy(value = f(value))

    override def flatMap[R](f: T => TryCatch[R, U]): TryCatch[R, U] = f(value)

    override def resolve[R](onValue: T => R, onError: U => R): R = onValue(value)
  }

  case class TryError[T, U](error: U) extends TryCatch[T, U] {
    override def map[R](f: T => R): TryCatch[R, U] = copy(error = error)

    override def flatMap[R](f: T => TryCatch[R, U]): TryCatch[R, U] = copy(error = error)

    override def resolve[R](onValue: T => R, onError: U => R): R = onError(error)
  }


}


