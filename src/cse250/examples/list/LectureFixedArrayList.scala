package cse250.examples.list

import scala.reflect.ClassTag

class LectureFixedArrayList[A: ClassTag](private val _capacity: Int = 10) extends collection.mutable.Seq[A]
  with cse250.examples.types.ListADT[A] {

  val _storage = new Array[A](_capacity)
  var _myLength = 0

  override def apply(idx: Int): A = {
    require(idx < _myLength)
    _storage(idx)
  }

  override def update(idx: Int, elem: A): Unit = {
    require(idx < _myLength)
    _storage(idx) = elem
  }

  override def insert(idx: Int, elem: A): Unit = {
    require(0 <= idx && idx <= _myLength)
    // What about idx < 0?
    // What about myLength < capacity?
    // Shift things right.
    for (i <- _myLength - 1 to idx by -1) _storage(i + 1) = _storage(i)
    // Place elem at index idx.
    _storage(idx) = elem
    // Increase myLength.
    _myLength += 1
  }

  override def remove(idx: Int): A = {
    require(0 <= idx && idx < _myLength)
    // Store reference to value to return later.
    val retval = _storage(idx)
    // Shift things left.
    for (i <- idx until _myLength - 1) _storage(i) = _storage(i + 1)
    // Decrease myLength.
    _myLength -= 1
    // Return removed value.
    retval
  }

  override def iterator: Iterator[A] = new Iterator[A] {
    var currPos = 0

    override def hasNext: Boolean = (currPos < _myLength)

    override def next(): A = {
      val retval = _storage(currPos)
      currPos += 1
      retval
    }
  }

  override def length: Int = _myLength
}
