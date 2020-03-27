package cse250.examples.adaptors

class LectureQueueArrayList[A]
  extends cse250.examples.types.mutable.QueueADT[A] {
  private val _store = new cse250.examples.list.LectureArrayList[A]

  override def enqueue(elem: A): Unit = _store.insert(_store.length, elem)

  override def front: A = _store(0)

  override def dequeue: A = _store.remove(0)

  override def isEmpty: Boolean = _store.length == 0
}
