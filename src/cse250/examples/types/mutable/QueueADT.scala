package cse250.examples.types.mutable

trait QueueADT[A] {
  def enqueue(elem: A): Unit

  def front: A

  def dequeue: A

  def isEmpty: Boolean
}
