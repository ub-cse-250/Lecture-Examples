package cse250.examples.list

class LectureDoublyLinkedList[A] extends collection.mutable.Seq[A]
  with cse250.examples.types.mutable.ListADT[A] {

  class DNode[A](var _value: A, var _next: DNode[A], var _prev: DNode[A])

  private var _headNode: DNode[A] = null
  private var _tailNode: DNode[A] = null
  private var _numStored: Int = 0

  override def apply(idx: Int): A = {
    require(0 <= idx && idx < _numStored)
    if (idx == _numStored - 1) {
      //Retrieve the tail node value
      _tailNode._value
    } else {
      //Retrieve the node value at idx
      var currentNode = _headNode
      for (_ <- 0 until idx) currentNode = currentNode._next
      currentNode._value
    }
  }

  override def update(idx: Int, elem: A): Unit = {
    require(0 <= idx && idx < _numStored)
    if (idx == _numStored - 1) {
      //Update the tail node value
      _tailNode._value = elem
    } else {
      //Update the node value at idx
      var currentNode = _headNode
      for (_ <- 0 until idx) currentNode = currentNode._next
      currentNode._value = elem
    }
  }

  override def insert(idx: Int, elem: A): Unit = {
    require(0 <= idx && idx <= _numStored)
    // List empty
    if (_numStored == 0) {
      _headNode = new DNode(elem, null, null)
      _tailNode = _headNode
      _numStored += 1
    }
    // List non-empty
    else {
      // Index 0 (new head)
      if (idx == 0) {
        _headNode = new DNode(elem, _next = _headNode, _prev = null)
        _headNode._next._prev = _headNode
      }
      // Between two nodes (not head, not tail)
      else if (idx < _numStored) {
        var currentNode = _headNode
        for (_ <- 0 until idx - 1) currentNode = currentNode._next
        currentNode._next = new DNode(elem, currentNode._next, currentNode)
      }
      // Index _numStored (new tail)
      else {
        _tailNode._next = new DNode(elem, null, _tailNode)
        _tailNode = _tailNode._next
      }
      _numStored += 1
    }
  }

  override def remove(idx: Int): A = {
    require(0 <= idx && idx < _numStored)
    // List is 1 item.
    if (_numStored == 1) {
      val retval = _headNode._value
      _headNode = null
      _tailNode = _headNode
      _numStored -= 1
      retval
    }
    // List non-empty
    else {
      // Index 0 (remove head)
      if (idx == 0) {
        val retval = _headNode._value
        _headNode = _headNode._next
        _headNode._prev = null
        _numStored -= 1
        retval
      }
      // Index _numStored-1 (remove tail)
      else if (idx == _numStored - 1) {
        val retval = _tailNode._value
        _tailNode = _tailNode._prev
        _tailNode._next = null
        _numStored -= 1
        retval
      }
      // Not removing head or tail.
      else {
        var currentNode = _headNode
        for (_ <- 0 until idx - 1) currentNode = currentNode._next
        val retval = currentNode._next._value
        currentNode._next = currentNode._next._next
        currentNode._next._prev = currentNode // Unsafe code at tail. Why?
        _numStored -= 1
        retval
      }
    }
  }

  override def iterator: Iterator[A] = new Iterator[A] {
    var _currentNode = _headNode
    override def hasNext: Boolean = _currentNode != null

    // Valid as long as hasNext is true.
    override def next: A = {
      val retval = _currentNode._value
      _currentNode = _currentNode._next
      retval
    }
  }

  override def length: Int = _numStored
}
