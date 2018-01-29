package storage.service

trait Storage{

}

class StorageMap(input : Map[String, Storage]) extends Storage {
  def get(key : String) : Storage = {
    input(key)
  }

  override def toString: String = input.toString
}

class StorageWrapper(value : Any) extends Storage {
  def getAs[T] : T = {
    value.asInstanceOf[T]
  }

  override def toString: String = value.toString
}

trait Reader[T] {
  def read(storage: Storage) : T
}

trait Writer[T] {
  def write(value : T) : Storage
}

trait Service[T] extends Reader[T] with Writer[T]

class ListReader[T](elementReader: Service[T]) extends Service[List[T]] {
  override def read(storage: Storage) : List[T] = {
    storage.asInstanceOf[StorageWrapper].getAs[List[Storage]].map(elementReader.read)
  }

  override def write(value: List[T]): Storage = {
    new StorageWrapper(value.map(elementReader.write))
  }
}

class IntReader extends Service[Int] {
  override def read(storage: Storage) : Int = {
    storage.asInstanceOf[StorageWrapper].getAs[Int]
  }

  override def write(value: Int): Storage = {
    new StorageWrapper(value)
  }
}

class StringReader extends Service[String] {
  override def read(storage: Storage) : String = {
    storage.asInstanceOf[StorageWrapper].getAs[String]
  }

  override def write(value: String): Storage = {
    new StorageWrapper(value)
  }
}

class StorageObject[R, T1, T2](
                                f : ((T1, T2) => R, R => Option[(T1, T2)]),
                                reader1: (String, Service[T1]),
                                reader2: (String, Service[T2])) extends Service[R] {

  override def read(storage: Storage): R = {
    val storageMap = storage.asInstanceOf[StorageMap]

    val tupledResult =
      (
        applyReader(reader1, storageMap),
        applyReader(reader2, storageMap)
      )

    f._1(tupledResult._1, tupledResult._2)
  }

  override def write(value: R): Storage = {
    val (v1, v2) = f._2(value).get

    val tupledResult =
      List(
        applyWriter(reader1, v1),
        applyWriter(reader2, v2)
      )

    new StorageMap(tupledResult.toMap)
  }


  private def applyReader[T](reader : (String, Reader[T]), storageMap: StorageMap) = {
    reader._2.read(storageMap.get(reader._1))
  }

  private def applyWriter[T](writer : (String, Writer[T]), value : T) : (String, Storage) = {
    writer._1 -> writer._2.write(value)
  }

}

case class Values(numbers : List[Int], names : List[String])

object Combinators {

  def list[T](elementReader: Service[T]): ListReader[T] = new ListReader(elementReader)

  def int = new IntReader

  def string = new StringReader

  def some[R, T1, T2](
                       f: ((T1, T2) => R, R => Option[(T1, T2)]),
                       reader1: (String, Service[T1]),
                       reader2: (String, Service[T2])): StorageObject[R, T1, T2] = new StorageObject(f, reader1, reader2)

}

object ValuesService {

  import Combinators._

  private val model =
    some(
      (Values.apply _, Values.unapply _),
      "numbers" -> list(int),
      "names" -> list(string)
    )

  def test() {

    val values = Values(List(1, 2, 3), List("a", "b", "c"))

    val storage = model.write(values)
    println(storage)

    val result = model.read(storage)
    println(result)
  }
}
