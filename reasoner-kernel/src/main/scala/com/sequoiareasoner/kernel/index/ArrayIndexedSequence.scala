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

package com.sequoiareasoner.kernel.index

import scala.collection.AbstractIterator
import scala.compat.Platform.ConcurrentModificationException

/** This call implements an [[IndexedSeq]] backed by an array.
  *
  * The underlying array is instantiated using a known type of `A` rather than `AnyRef`. This helps with profiling
  * memory using since the type of objects in the array will be exposed to profiling tools.
  *
  * The capacity of the underlying array grows automatically as elements are added to the indexed sequence, and hence
  * its capacity is always at least as large as the indexed sequence. Adding an element has amortized time cost.
  *
  * This implementation of this indexed sequence is not thread safe.
  *
  * If multiple threads concurrently access an instance of this class, and at least one of the threads adds or removes
  * an element, then access must be synchronized externally.
  *
  * The iterator returned by this class is ''fail-fast'': if an element is added or removed at any time after the
  * iterator is created, the iterator will throw a [[scala.compat.Platform.ConcurrentModificationException]] on a
  * best-effort basis. Hence, the fail-fast behavior of the iterator should be used only to detect bugs.
  *
  * The purpose of `makeElementData` is to provide the runtime type of the array to profiling tools without the use of
  * reflection and Scala TypeTags.
  *
  * @param elementData      the array in which the elements are stored.
  * @param numElements      the number of elements contained in this indexed sequence.
  * @param makeElementData  a function f(n) that returns an array of length n.
  */
final class ArrayIndexedSequence[A] private (private[this] var elementData: Array[A],
                                             private[this] var numElements: Int,
                                             makeElementData: Int => Array[A])
  extends MutableIndexedSequence[A] with CollectionMakeString {
  self =>

  /** Constructs an empty indexed sequence with the specified initial capacity.
    *
    * @param initialCapacity  the initial capacity of the list
    */
  def this(initialCapacity: Int, makeElementData: Int => Array[A]) =
    this(makeElementData(initialCapacity), 0, makeElementData)

  /** The number of times this list has been structurally modified. A structural modification is any operation that will
    * change of the length of this indexed sequence, and hence includes adding and removing elements. Such changes can
    * cause iterations in progress may to yield incorrect results.
    *
    * This field is used to provide a fail-fast iterator implementation.
    */
  private[this] var modCount: Int = 0

  private[this] var foreachCursor: Int = -1

  override def length: Int = numElements

  override def isEmpty: Boolean = numElements == 0

  override def nonEmpty: Boolean = numElements > 0

  /** Increases the capacity of the array so that it can hold at least `minCapacity` elements.
    *
    * @param minCapacity the desired minimum capacity
    */
  private[this] def grow(minCapacity: Int): Unit = {
    val oldCapacity: Int = elementData.length
    var newCapacity: Int = oldCapacity + (oldCapacity >> 1)
    if (newCapacity - minCapacity < 0) newCapacity = minCapacity
    val newArray = makeElementData(newCapacity)
    System.arraycopy(elementData, 0, newArray, 0, oldCapacity)
    elementData = newArray
  }

  private[this] def ensureExplicitCapacity(minCapacity: Int): Unit = {
    modCount += 1
    if (minCapacity - elementData.length > 0) grow(minCapacity)
  }

  private[this] def fastRemove(index: Int): Unit = {
    modCount += 1
    // If deleting at or before the cursor, then keep cursor in same position so not to skip an element.
    if (foreachCursor != -1 && foreachCursor >= index) foreachCursor -= 1
    val numMoved: Int = numElements - index - 1
    if (numMoved > 0) System.arraycopy(elementData, index + 1, elementData, index, numMoved)
    numElements -= 1
    elementData(numElements) = null.asInstanceOf[A]
  }

  /** Returns the index of the first occurrence of the specified element in this indexed sequence, or -1 if this indexed
    * sequence does not contain the element.
    *
    * More formally, returns the lowest index `i` such that `o == get(i)`, or -1 if there is no such index.
    */
  private[this] def indexOf(o: A): Int = {
    require(o != null)
    val length = elementData.length
    var i = 0
    while (i < length) {
      if (o == elementData(i)) return i
      i += 1
    }
    return -1
  }

  private[this] def prefixLength(p: A => Boolean, expectTrue: Boolean): Int = {
    val length = numElements
    var i = 0
    while (i < length && p(elementData(i)) == expectTrue) i += 1
    i
  }

  override def forall(p: A => Boolean): Boolean = prefixLength(p, expectTrue = true) == numElements
  override def exists(p: A => Boolean): Boolean = prefixLength(p, expectTrue = false) != numElements
  override def contains(o: A): Boolean = indexOf(o) >= 0

  override def apply(index: Int): A = {
    // No need to check if index < 0 because this is done by the array access.
    if (index >= numElements) throw new IndexOutOfBoundsException
    elementData(index)
  }

  /** Applies a function `f` to all elements of this array buffer.
    *
    * This method supports traversal and concurrent modification.
    *
    * @param f the function that is applied for its side-effect to every element. The result of function f is discarded.
    * @tparam U
    */
  override def foreach[U](f: A => U): Unit = {
    foreachCursor = 0
    while (foreachCursor < numElements) {
      f(elementData(foreachCursor))
      foreachCursor += 1
    }
    foreachCursor = -1
  }

  /** Appends the specified element to the end of this indexed sequence.
    *
    * @param e  element to be appended to this indexed sequence.
    * @return the indexed sequence itself
    */
  override def +=(e: A): this.type = {
    ensureExplicitCapacity(numElements + 1)
    elementData(numElements) = e
    numElements += 1
    this
  }

  /** Removes the first occurrence of the specified element from this indexed sequence. Otherwise, if the indexed
    * sequence does not contain the element, it is unchanged.
    *
    * More formally, removes the element with the lowest index `i` such that `o == get(i)` (if such an element exists).
    *
    * @param o element to be removed from this list, if present
    * @return the indexed sequence itself
    */
  override def -=(o: A): this.type = {
    require(o != null)
    var index: Int = 0
    while (index < numElements) {
      if (o == elementData(index)) {
        fastRemove(index)
        return this
      }
      index += 1
    }
    return this
  }

  override def clear: Unit = {
    modCount += 1
    numElements = 0
    elementData = makeElementData(16)
  }

  /** Returns an fail-fast iterator over the elements in this list in proper sequence.
    *
    * If this indexed sequence is structurally modified before the iterator is exhausted, then the returned iterator
    * will throw a `ConcurrentModificationException` when the `next` method is called.
    *
    * @return an fail-fast iterator over the elements in this list in proper sequence.
    */
  override def iterator: Iterator[A] = new AbstractIterator[A] {
    private[this] var cursor: Int = 0
    private[this] val expectedModCount: Int = modCount
    def hasNext: Boolean = cursor != self.numElements
    def next: A = {
      if (modCount != expectedModCount) throw new ConcurrentModificationException
      val i: Int = cursor
      if (i >= self.numElements) throw new NoSuchElementException
      val elementData: Array[A] = self.elementData
      cursor = i + 1
      elementData(i)
    }
  }

  override def copy: ArrayIndexedSequence[A] = new ArrayIndexedSequence[A](elementData.clone, numElements, makeElementData)

  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    b.append(start)
    var first = true
    var i = 0
    while (i < numElements) {
      if (!first) b.append(sep)
      first = false
      b.append(elementData(i))
      i += 1
    }
    b.append(end)
    b
  }

  override def toString: String = addString(new StringBuilder, "ArrayIndexedSequence[", ", ", "]").result

  override def hashCode: Int = {
    // Implement the definition of hashCode specified by the superclass.
    var result = 1
    var idx = 0
    while (idx < numElements) {
      val elem = elementData(idx)
      result = 31*result + (if (elem == null) 0 else elem.hashCode)
      idx += 1
    }
    result
  }

  override def equals(o: Any): Boolean = o match {
    case that: IndexedSequence[_] if that.length == numElements =>
      var idx = 0
      var result = true
      while (result && idx < numElements) {
        result = elementData(idx) == that(idx)
        idx += 1
      }
      result
    case _ => false
  }

}
