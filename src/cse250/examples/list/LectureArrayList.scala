package cse250.examples.list

import scala.reflect.ClassTag

class LectureArrayList[A: ClassTag](private var _capacity: Int = 10) extends collection.mutable.Seq[A]
  with cse250.examples.types.ListADT[A] {

  var _storage = new Array[A](_capacity)
  var _myLength = 0

  override def apply(idx: Int): A = {
    require(idx < _myLength)
    _storage(idx)
  }

  override def update(idx: Int, elem: A): Unit = {
    require(idx < _myLength)
    _storage(idx) = elem
  }

  def reserve(newCapacity: Int): Unit = {
    if (_capacity < newCapacity) {
      val oldStorage = _storage
      _storage = new Array[A](newCapacity)
      _capacity = newCapacity
      for (i <- 0 until oldStorage.length) _storage(i) = oldStorage(i)
    }
  }

  override def insert(idx: Int, elem: A): Unit = {
    require(0 <= idx && idx <= _myLength)
    // What about idx < 0?
    // What about myLength < capacity?

    // If capacity is reached, reserve more space.
    if (_myLength == _capacity) reserve(2*_capacity max 1)

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
    // Do we want to shrink if too much unused space?
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
