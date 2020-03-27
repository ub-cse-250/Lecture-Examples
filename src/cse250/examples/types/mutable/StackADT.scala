package cse250.examples.types.mutable

trait StackADT[A] {
  def push(elem: A): Unit

  def top: A

  def pop: A

  def isEmpty: Boolean
}
