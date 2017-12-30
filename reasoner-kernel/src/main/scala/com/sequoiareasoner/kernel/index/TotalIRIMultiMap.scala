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
 * Copyright (c) 2016, Andrew Bate, University of Oxford <andrew.bate@cs.ox.ac.uk>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the copyright holder nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.sequoiareasoner.kernel.index

import com.sequoiareasoner.kernel.owl.iri.IRI

import scala.collection.AbstractIterator
import scala.compat.Platform.ConcurrentModificationException

object TotalIRIMultiMap {
  private final val initialIndexCapacity = 10
  private final val initialBucketCapacity = 5
}

/** A map of key/value pairs in which each key is an IRI.
  *
  * The implementation assumes that the UID of IRIs are allocated sequentially, starting from zero.
  * This data structure then uses perfect hashing from the UID of IRIs to clauses.
  *
  * The iterator returned by this class is ''fail-fast'': if a key is added or removed at any time after the iterator is
  * created, the iterator will throw a [[scala.compat.Platform.ConcurrentModificationException]] on a best-effort basis.
  * Hence, the fail-fast behavior of the iterator should be used only to detect bugs.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param elementData      the array into which the values of the map will be stored.
  * @param makeElementData  a function f(n) that returns an Array[A] of length n
  */
final class TotalIRIMultiMap[A] private(private[this] var elementData: Array[MutableIndexedSequence[A]],
                                        makeElementData: Int => Array[A]) {
  import TotalIRIMultiMap._

  def this(makeElementData: Int => Array[A]) =
    this(new Array[MutableIndexedSequence[A]](TotalIRIMultiMap.initialIndexCapacity), makeElementData: Int => Array[A])

  /** The number of times this map has been structurally modified. A structural modification is any operation that will
    * change of the length of this indexed sequence, and hence includes adding and removing elements. Such changes can
    * cause iterations in progress may to yield incorrect results.
    *
    * This field is used to provide a fail-fast iterator implementation.
    */
  private[this] var modCount: Int = 0

  /** Increases the capacity of this `ArrayList` instance, if necessary, to ensure that it can hold at least the number
    * of elements specified by the minimum capacity argument.
    *
    * @param minCapacity the desired minimum capacity
    */
  private[this] def ensureExplicitCapacity(minCapacity: Int): Unit = {
    modCount += 1
    if (minCapacity - elementData.length > 0) grow(minCapacity)
  }

  /** Increases the capacity to ensure that it can hold at least the number of elements specified by the minimum
    * capacity argument.
    *
    * @param minCapacity the desired minimum capacity
    */
  private[this] def grow(minCapacity: Int): Unit = {
    val oldCapacity: Int = elementData.length
    var newCapacity: Int = oldCapacity + (oldCapacity >> 1)
    if (newCapacity - minCapacity < 0) newCapacity = minCapacity
    elementData = java.util.Arrays.copyOf(elementData, newCapacity)
  }

  /**
    * @return an fail-fast iterator over the elements in this list in proper sequence
    */
  private[this] def iterator: Iterator[Iterator[A]] = new AbstractIterator[Iterator[A]] {
    private[this] val expectedModCount: Int = modCount
    private[this] var cursor: Int = seekNextIndex(0)

    private[this] def seekNextIndex(start: Int): Int = {
      val length = elementData.length
      var i = start
      while (i < length && elementData(i) == null) { i += 1 }
      i
    }

    override def hasNext: Boolean = cursor < elementData.length

    override def next: Iterator[A] = {
      if (modCount != expectedModCount) throw new ConcurrentModificationException
      val i: Int = cursor
      val elementData: Array[MutableIndexedSequence[A]] = TotalIRIMultiMap.this.elementData
      if (i >= elementData.length) throw new NoSuchElementException
      cursor = seekNextIndex(i + 1)
      elementData(i).iterator
    }
  }

  /** Returns the element at the specified position in this list. // FIXME: update documentation
    *
    * @param iri  index of the element to return
    * @return the element at the specified position in this list
    */
  private[this] def get(iri: IRI): MutableIndexedSequence[A] =
    if (0 <= iri.uid && iri.uid < elementData.length) elementData(iri.uid)
    else null

  private[this] def getOrElseUpdate(iri: IRI): MutableIndexedSequence[A] = {
    val values = get(iri)
    if (values eq null) {
      val newValues = new ArrayIndexedSequence[A](initialBucketCapacity, makeElementData)
      ensureExplicitCapacity(iri.uid + 1)
      elementData(iri.uid) = newValues
      newValues
    } else {
      values
    }
  }

  /**
    * @return an fail-fast iterator over the elements in this list in proper sequence
    */
  def valuesIterator: Iterator[A] = iterator.flatten

  /**
    * Supports traversal and concurrent modification using the addBinding and removeBinding methods only.
    *
    * @param f
    * @tparam U
    */
  def foreachKeys[U](f: IRI => U): Unit = {
    val length = elementData.length
    var uid = 0
    while (uid < length) {
      val elem = elementData(uid)
      if (elem ne null) f(new IRI(uid))
      uid += 1
    }
  }

  /**
    *
    * Supports traversal and concurrent modification using the addBinding and removeBinding methods only.
    *
    * @param iri
    * @param f
    * @tparam U
    */
  def foreach[U](iri: IRI)(f: A => U): Unit = {
    require(iri.uid >= 0)
    val currentValues: MutableIndexedSequence[A] = get(iri)
    if (currentValues ne null) {
      currentValues.foreach(f)
    }
  }

  def addBinding(iri: IRI, v: A): Unit = {
    require(iri.uid >= 0)
    require(v != null)
    val values = getOrElseUpdate(iri)
    values += v
  }

  def addKey(iri: IRI): MutableIndexedSequence[A] = {
    require(iri.uid >= 0)
    getOrElseUpdate(iri) // Maintains the mod count.
  }

  /**
    * Removing a binding leaves the key present in the collection.
    *
    * @param iri
    * @param v
    */
  def removeBinding(iri: IRI, v: A): Unit = {
    require(iri.uid >= 0)
    val values = get(iri)
    if (values ne null) {
      values -= v
    }
  }

  def removeKey(iri: IRI): Unit = {
    require(iri.uid >= 0)
    val values = get(iri)
    if (values ne null) {
      modCount += 1
      elementData(iri.uid) = null
    }
  }

  def apply(iri: IRI): IndexedSequence[A] = {
    require(iri.uid >= 0)
    val values = get(iri)
    if (values ne null) {
      values
    } else {
      IndexedSequence.empty[A]
    }
  }

  def copy: TotalIRIMultiMap[A] = {
    val expectedModCount: Int = modCount
    val length = elementData.length
    val newElementData = new Array[MutableIndexedSequence[A]](length)
    var i: Int = 0
    while (i < length) {
      val elem = elementData(i)
      newElementData(i) =
        if (elem eq null) null
        else elem.copy
      i += 1
    }
    if (modCount != expectedModCount) throw new ConcurrentModificationException
    new TotalIRIMultiMap(newElementData, makeElementData)
  }

}
