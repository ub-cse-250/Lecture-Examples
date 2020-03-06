package cse250.examples.types

trait ListADT[A] {
  /** Gets the element at the specified index. */
  def apply(idx: Int): A

  /** Replaces element at given index with a new value. */
  def update(idx: Int, elem: A): Unit

  /** Inserts element at given index. */
  def insert(idx: Int, elem: A): Unit

  /** Removes element at given index and returns the value removed. */
  def remove(idx: Int): A

  /** Returns an Iterator that can be used only once.
   *  Get access to view all elements, in order, contained within the sequence. */
  def iterator: Iterator[A]

  override def toString: String = iterator.toList.addString(new StringBuilder,",").result()
}
