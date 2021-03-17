package cse250.examples.adaptors

import scala.reflect.ClassTag

class LectureQueueArrayList[A: ClassTag]
  extends cse250.examples.types.mutable.QueueADT[A] with Seq[A] {
  private val _store = new cse250.examples.list.LectureArrayList[A]

  override def enqueue(elem: A): Unit = _store.insert(_store.length, elem)

  override def front: A = _store(0)

  override def dequeue(): A = _store.remove(0)

  override def isEmpty: Boolean = _store.length == 0

  override def length: Int = _store.length

  override def apply(idx: Int): A = _store(idx)

  override def iterator = _store.iterator
}
