package cse250.examples.list

class LecturePositionalLinkedList[A] extends collection.mutable.Seq[A]
  with cse250.examples.types.mutable.ListADT[A] {

  class DNode[A](var _value: A, var _next: DNode[A], var _prev: DNode[A])

  class Position(var _currentNode: DNode[A]) extends Iterator[A] {
    override def hasNext: Boolean = _currentNode != null

    // Valid as long as hasNext is true.
    override def next: A = {
      val retval = _currentNode._value
      _currentNode = _currentNode._next
      retval
    }

    def hasPrev: Boolean = (_currentNode != null && _currentNode._prev != null) || (_currentNode == null && _tailNode != null)

    // Valid as long as hasPrev is true.
    def prev: A = {
      // If we moved off the end of the list, move back to tail.
      if (_currentNode == null) _currentNode = _tailNode
      // Otherwise, move back one node.
      else _currentNode = _currentNode._prev
      _currentNode._value
    }

    // Valid as long as hasNext is true.
    def value: A = _currentNode._value
  }

  private var _headNode: DNode[A] = null
  private var _tailNode: DNode[A] = null
  private var _numStored: Int = 0

  override def apply(idx: Int): A = {
    require(0 <= idx && idx < _numStored)
    var currentNode = _headNode
    for (_ <- 0 until idx) currentNode = currentNode._next
    currentNode._value
  }

  def position(idx: Int): Position = {
    require(0 <= idx && idx < _numStored)
    var currentNode = _headNode
    for (_ <- 0 until idx) currentNode = currentNode._next
    new Position(currentNode)
  }

  override def update(idx: Int, elem: A): Unit = {
    require(0 <= idx && idx < _numStored)
    var currentNode = _headNode
    for (_ <- 0 until idx) currentNode = currentNode._next
    currentNode._value = elem
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
        currentNode._next = new DNode(elem, _next = currentNode._next, _prev = currentNode)
      }
      // Index _numStored (new tail)
      else {
        _tailNode._next = new DNode(elem, _next = null, _prev = _tailNode)
        _tailNode = _tailNode._next
      }
      _numStored += 1
    }
  }

  def insert(pos: Position, elem: A): Unit = {
    // Inserting the element after pos.next.
    require(pos.hasNext)
    // posNextNode holds the value we are inserting before.
    val posNextNode = pos._currentNode._next
    val newNode = new DNode(_value = elem, _next = posNextNode, _prev = pos._currentNode)
    // Link in new node after position.
    pos._currentNode._next = newNode
    // Update tail or link posNextNode back to newNode.
    if (newNode == null) _tailNode = newNode
    else posNextNode._prev = newNode

    _numStored += 1
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

  def remove(pos: Position): Unit = {
    // Removing the element after pos.next.
    require(pos.hasNext)
    val posNextNode = pos._currentNode._next
    // Link around node after pos._currentNode.
    if (posNextNode == _tailNode) {
      pos._currentNode._next = null
      _tailNode = pos._currentNode
    }
    else {
      pos._currentNode._next = posNextNode._next
      posNextNode._next._next = pos._currentNode
    }

    _numStored -= 1
  }

  override def iterator: Iterator[A] = new Position(_headNode)

  override def length: Int = _numStored
}
