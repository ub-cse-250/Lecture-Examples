/**
 * cse250.examples.types.mutable.PriorityQueueADT.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.examples.types.mutable

/**
 * An interface for a mutable priority queue.
 *
 * @param _ordering an ordering that compares keys associated with
 *                  elements of type A.
 * @tparam A type of elements being stored. Elements should either be
 *           keys themselves or have keys derivable from the elements.
 */
abstract class PriorityQueueADT[A](implicit _ordering: Ordering[A]) {
  /**
   * Inserts element into the priority queue.
   *
   * @param elem the element to add into the priority queue.
   */
  def enqueue(elem: A): Unit

  /**
   * Gets an element with a key of highest priority.
   *
   * @return an element eMax such that _ordering.lt(eMax,e) is false
   *         for every stored element e.
   */
  def head: A

  /** Removes and returns the element referenced by head. */
  def dequeue(): A

  /** Returns true if any elements are stored in the priority queue.   */
  def isEmpty: Boolean
}
