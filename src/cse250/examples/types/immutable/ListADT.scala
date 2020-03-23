package cse250.examples.types.immutable

trait ListADT[+A] {
  /** Gets the element at the specified index. */
  def apply(idx: Int): A

  /** Returns a copy of the list with the element replaced at the given index. */
  def updated[B>:A](idx: Int, elem: B): ListADT[B]

  /** Returns a copy of the list with the element inserted at the given index. */
  def inserted[B>:A](idx: Int, elem: B): ListADT[B]

  /** Returns a copy of the list with the element removed at the given index. */
  def removed(idx: Int): ListADT[A]

  /** Returns an Iterator that can be used only once.
   *  Get access to view all elements, in order, contained within the sequence. */
  def iterator: Iterator[A]

  override def toString: String = iterator.toList.addString(new StringBuilder,"ListADT(",",",")").result()
}