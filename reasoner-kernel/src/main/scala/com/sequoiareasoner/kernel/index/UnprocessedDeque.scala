/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This file is part of Sequoia, an OWL 2 reasoner that supports the SRIQ subset of OWL 2 DL.
 * Copyright 2017 by Andrew Bate <code@andrewbate.com>.
 *
 * This code is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 only,
 * as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License version 3 for more details (a copy is
 * included in the LICENSE file that accompanied this code).
 *
 * You should have received a copy of the GNU General Public License
 * version 3 along with this work.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * This file is available under and governed by the GNU General Public
 * License version 3 only, as published by the Free Software Foundation.
 * However, the following notice accompanied the original version of this
 * file:
 *
 * Written by Josh Bloch of Google Inc. and released to the public domain,
 * as explained at http://creativecommons.org/licenses/publicdomain.
 */

package com.sequoiareasoner.kernel.index

import com.sequoiareasoner.kernel.clauses.ContextClause

import scala.collection.AbstractIterator
import scala.compat.Platform.ConcurrentModificationException

/** A sequence of context clauses that supports insertion and removal at both ends. This data structure also supports
  * the removal of redundant clauses. Null clauses are prohibited.
  *
  * The implementation is backed by a resizable-array. Most operations, including the methods to add and remove single
  * clauses, run in amortized constant time. Removal of redundant clauses and checking if a clause exists in the
  * sequence require linear time.
  *
  * This data structure is not thread-safe.
  *
  * The iterator returned by this class is ''fail-fast'': If clauses are added or removed at any time after the iterator
  * is created, the iterator will throw a [[scala.compat.Platform.ConcurrentModificationException]] on a best-effort
  * basis. Hence, the fail-fast behavior of the iterator should be used only to detect bugs.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class UnprocessedDeque extends CollectionMakeString {

  private[this] final val initialCapacity = 16

  /** The array to store the clauses of the deque. Length must be a power of two. The capacity of the deque is the
    * length of this array. If this array ever becomes full, it must be resized immediately (using doubleCapacity),
    * thus avoiding head and tail wrapping around to equal each other.
    */
  private[this] var clauses = new Array[ContextClause](initialCapacity)

  /** INVARIANT: all cells of `clauses` not holding clauses are `null`. */
  private[this] def checkInvariants(): Unit =
    assert {
      // Element at tail must be equal to null.
      (clauses(tail) eq null) &&
      // Element one before head must be null.
      (clauses((head - 1) & (clauses.length - 1)) eq null) &&
      // Either the dequeue is empty, or the head is non-null and the one before the tail is non-null.
      (head == tail || ((clauses(head) ne null) && (clauses((tail - 1) & (clauses.length - 1)) ne null)))
    }

  /** The index of the element at the head of the deque (which is the element that would be removed by `removeFirst`);
    * or an arbitrary number equal to tail if the deque is empty.
    */
  private[this] var head: Int = 0

  /** The index at which the next element would be added to the tail of the deque via `addLast`. */
  private[this] var tail: Int = 0

  /** Double the size of the array. Only to be called when full (i.e., when head and tail have wrapped around to become equal). */
  private[this] def doubleCapacity: Unit = {
    assert(head == tail)
    val p: Int = head
    val n: Int = clauses.length
    val r: Int = n - p // The number of clauses to the right of p.
    val newCapacity: Int = n << 1
    if (newCapacity < 0)
      throw new IllegalStateException("UnprocessedDeque too big!")
    val a = new Array[ContextClause](newCapacity)
    System.arraycopy(clauses, p, a, 0, r)
    System.arraycopy(clauses, 0, a, r, p)
    clauses = a
    head = 0
    tail = n
  }

  /** If this deque is empty, then shrink the capacity of the array to the initial capacity. */
  private[this] def checkResetCapacity: Unit =
    if (head == tail && clauses.length > initialCapacity) {
      clauses = new Array[ContextClause](initialCapacity)
      head = 0
      tail = 0
    }

  /** Pushes a clause onto the stack represented by this deque (i.e., inserts the clause at the front of this deque).
    *
    * @param c the context clause to add.
    * @throws NullPointerException if `c` is `null`.
    */
  def addFirst(c: ContextClause): Unit = {
    if (c eq null) throw new NullPointerException
    val newHead = (head - 1) & (clauses.length - 1)
    clauses(newHead) = c
    head = newHead // Maintain invariants *before* call to doubleCapacity.
    if (newHead == tail) doubleCapacity
  }

  /** Enqueues a clause onto the queue represented by this deque (i.e., inserts the clause at the end of this deque).
    *
    * @param c the context clause to add.
    * @throws NullPointerException if `c` is `null`.
    */
  def addLast(c: ContextClause): Unit = {
    if (c eq null) throw new NullPointerException
    clauses(tail) = c
    val newTail = (tail + 1) & (clauses.length - 1)
    tail = newTail // Maintain invariants *before* call to doubleCapacity.
    if (newTail == head) doubleCapacity
  }

  /** Pops (respectively, dequeues) an element from the stack (respectively, queue) represented by this deque (i.e.,
    * removes and returns the first element of this deque).
    *
    * @return the clause at the front of this deque (which is the top of the stack or queue represented by this deque).
    * @throws NoSuchElementException if this deque is empty.
    */
  def removeFirst: ContextClause = {
    val h: Int = head
    val result: ContextClause = clauses(h)
    // Element is null if deque empty.
    if (result eq null) throw new NoSuchElementException
    clauses(h) = null // Must null out slot to maintain class invariant.
    head = (h + 1) & (clauses.length - 1)
    checkResetCapacity
    result
  }

  /** Retrieves and removes the last element of this deque.
    *
    * @return the clause at the end of this deque.
    * @throws NoSuchElementException if this deque is empty.
    */
  def removeLast: ContextClause = {
    val t: Int = (tail - 1) & (clauses.length - 1)
    val result: ContextClause = clauses(t)
    if (result eq null) throw new NoSuchElementException
    clauses(t) = null
    tail = t
    checkResetCapacity
    result
  }

  /** Remove from this deque all clauses `d` such that `c` is a proper strengthening of `d`. Returns `true` if and only
    * if no clause in the deque is a strengthening of or equal to `c`.
    *
    * @param c the clause against which to check for redundancy.
    * @return `true` if and only if no clause in the deque is a strengthening of or equal to `c`.
    */
  def removeRedundant(c: ContextClause): Boolean = {
    checkInvariants()
    val clauses: Array[ContextClause] = this.clauses
    val tail = this.tail
    var r = head
    var w = head
    val mask = clauses.length - 1
    while (r != tail) {
      val cmp = clauses(r).testStrengthening(c)
      if (cmp < 0) {
        if (r != w)
          throw new IllegalStateException("A clause this sequence exists that is a strengthening of another clause in this sequence!")
        return false
      } else if (cmp == 0) {
        // Only retain the clauses `d` for which neither `c` nor `d` is a strengthening of the other.
        clauses(w) = clauses(r)
        w = (w + 1) & mask
      }
      r = (r + 1) & mask
    }
    var i = w
    while (i != tail) {
      clauses(i) = null
      i = (i + 1) & mask
    }
    this.tail = w
    checkResetCapacity
    return true
  }

  /**
    * @return the number of clauses in this deque.
    */
  def size: Int = (tail - head) & (clauses.length - 1)

  /**
    * @return `true` if and only if this deque contains no clauses.
    */
  def isEmpty: Boolean = head == tail

  /**
    * @return `true` if and only if this deque contains at least one clause.
    */
  def nonEmpty: Boolean = head != tail

  /** Returns an iterator over the clauses in this deque.
    *
    * The clauses will be ordered from first (head) to last (tail). This is the same order that clauses would be
    * dequeued or popped (via successive calls to [[removeFirst]]).
    *
    * @return an iterator over the clauses in this deque.
    */
  def iterator: Iterator[ContextClause] = new AbstractIterator[ContextClause] {

    /** Index of element to be returned by subsequent call to next. */
    private[this] var cursor: Int = head

    /** Tail recorded at construction to stop iterator and also to check for concurrent modification. */
    private[this] val fence: Int = tail

    override def hasNext: Boolean = cursor != fence

    override def next: ContextClause = {
      if (cursor == fence) throw new NoSuchElementException
      val result: ContextClause = clauses(cursor)
      /* The following check does not catch all possible concurrent modifications, but does catch the ones that
       * corrupt traversal. */
      if (tail != fence || result == null) throw new ConcurrentModificationException
      cursor = (cursor + 1) & (clauses.length - 1)
      result
    }

  }

  /** Returns `true` if this deque contains the specified element. More formally, returns `true` if and only if this
    * deque contains at least one element `e` such that `o == e`.
    *
    * @param c  the clause to be checked for containment in this deque.
    * @return `true` if this deque contains the specified element.
    */
  def contains(c: ContextClause): Boolean = {
    if (c eq null) return false
    val mask = clauses.length - 1
    var i = head
    var found = false
    while (i != tail) {
      val current = clauses(i)
      if (current eq null) throw new Error
      found = c == current
      i = (i + 1) & mask
    }
    found
  }

  /** Removes all of the clauses from this deque.
    * The deque will be empty after this call returns.
    */
  def clear: Unit = {
    val h: Int = head
    val t: Int = tail
    if (h != t) {
      // Clear all cells.
      head = 0
      tail = 0
      var i = h
      val mask = clauses.length - 1
      do {
        clauses(i) = null
        i = (i + 1) & mask
      } while (i != t)
    }
  }

  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    iterator.addString(b, start, sep, end)

  /** Returns a string representation of this sequence. The string representation consists of a list of the elements of
    * this sequence in order, enclosed in square brackets. Adjacent elements are separated by the characters `", "`
    * (comma and space).
    *
    * @return a string representation of this collection.
    */
  override def toString: String = iterator.mkString("[", ", ", "]")
}
