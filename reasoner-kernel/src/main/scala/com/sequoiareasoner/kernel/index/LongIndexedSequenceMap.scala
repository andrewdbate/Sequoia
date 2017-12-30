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
 * Copyright (c) 2002 - EPFL
 * Copyright (c) 2011 - Lightbend, Inc.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *    * Redistributions of source code must retain the above copyright notice, this
 *      list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright notice,
 *      this list of conditions and the following disclaimer in the documentation
 *      and/or other materials provided with the distribution.
 *    * Neither the name of the EPFL nor the names of its contributors may be used
 *      to endorse or promote products derived from this software without specific
 *      prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.sequoiareasoner.kernel.index

import scala.collection.{AbstractIterator, AbstractSet}

/** This class implements a mutable multi-map with `Long` keys based on a hash table with open addressing.
  * A multi-map is a map with multiple values assigned to a key.
  *
  * Maps with open addressing may become less efficient at lookup after repeated addition/removal of elements. Although
  * this class attempts to remain efficient regardless, calling `repack` n a map that will no longer have elements
  * removed but will be used heavily may save both time and storage space.
  *
  * This map is not intended to contain more than `2^29` entries (approximately 500 million). The maximum capacity is
  * `2^30`, but performance will degrade rapidly as `2^30` is approached.
  *
  * @constructor Creates a new `LongIndexedSequenceMap` with an initial buffer of specified size.
  *
  * A LongMap can typically contain half as many elements as its buffer size before it requires resizing.
  *
  * The purpose of `makeElementData` is to provide the runtime type of the array to profiling tools without the use of
  * reflection and Scala TypeTags.
  *
  * @param  initialBufferSize  the initial capacity of the buffer
  * @param  makeElementData a function f(n) that returns an array of length n
  */
final class LongIndexedSequenceMap[A >: Null <: AnyRef](initialBufferSize: Int, makeElementData: Int => Array[A])
  extends CollectionMakeString {
  self =>

  if (initialBufferSize < 0) throw new IllegalArgumentException("Illegal Capacity: " + initialBufferSize)

  private[this] final val IndexMask  = 0x3FFFFFFF
  private[this] final val MissingBit = 0x80000000
  private[this] final val VacantBit  = 0x40000000
  private[this] final val MissVacant = 0xC0000000

  // Mask is one less than the next largest power of two.
  private[this] var mask =
    (((1 << (32 - java.lang.Integer.numberOfLeadingZeros(initialBufferSize-1))) - 1) & 0x3FFFFFFF) | 0x7
  private[this] var _zeroValue: ArrayIndexedSequence[A] = null
  private[this] var _size = 0
  private[this] var _vacant = 0
  private[this] var _keys = new Array[Long](mask+1)
  private[this] var _values = new Array[ArrayIndexedSequence[A]](mask+1)

  private[this] def imbalanced: Boolean =
    (_size + _vacant) > 0.5*mask || _vacant > _size

  private[this] def assertLegalKey(k: Long): Unit = if (k == Long.MinValue) throw new IllegalArgumentException

  private[this] def toIndex(k: Long): Int = {
    // Part of the MurmurHash3 32 bit finalizer.
    val h = ((k ^ (k >>> 32)) & 0xFFFFFFFFL).toInt
    val x = (h ^ (h >>> 16)) * 0x85EBCA6B
    (x ^ (x >>> 13)) & mask
  }

  private[this] def seekEmpty(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    while (_keys(e) != 0) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
    e
  }

  private[this] def seekEntry(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q==k) return e; q != 0}) { x += 1; e = (e + 2*(x+1)*x - 3) & mask }
    e | MissingBit
  }

  private[this] def seekEntryOrOpen(k: Long): Int = {
    var e = toIndex(k)
    var x = 0
    var q = 0L
    while ({ q = _keys(e); if (q==k) return e; q+q != 0}) {
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    if (q == 0) return e | MissingBit
    val o = e | MissVacant
    while ({ q = _keys(e); if (q==k) return e; q != 0}) {
      x += 1
      e = (e + 2*(x+1)*x - 3) & mask
    }
    o
  }

  /** Retrieves the indexed sequence associated with a key, or null if none exists.
    *
    * This method is the same as apply, except it does not check if the key is legal and returns null if a value does
    * not exist (instead of fetching an empty indexed sequence). Hence this is the fastest way to retrieve a value.
    */
  private[this] def getOrNull(key: Long): ArrayIndexedSequence[A] = {
    if (key == 0) {
      _zeroValue
    } else {
      val i = seekEntry(key)
      if (i < 0) null else _values(i)
    }
  }

  /** Repacks the contents of this map for maximum efficiency of lookup.
    * Takes time proportional to the number of entries in the map.
    *
    * For maps that undergo a complex creation process with both addition and removal of keys, and then are used
    * heavily with no further removal of elements, calling `repack` after the end of the creation can result in
    * improved performance.
    */
  def repack: Unit = {
    val oldKeys = _keys
    val oldValues = _values
    var m = mask
    if (_size + _vacant >= 0.5*mask && !(_vacant > 0.2*mask)) m = ((m << 1) + 1) & IndexMask
    while (m > 8 && 8*_size < m) m = m >>> 1
    mask = m
    _keys = new Array[Long](mask+1)
    _values = new Array[ArrayIndexedSequence[A]](mask+1)
    _vacant = 0
    var i = 0
    while (i < oldKeys.length) {
      val k = oldKeys(i)
      if (k != -k) {
        val j = seekEmpty(k)
        _keys(j) = k
        _values(j) = oldValues(i)
      }
      i += 1
    }
  }

  /** Updates the map to include a new key-value pair.
    *
    * This is the fastest way to add an entry to this map.
    */
  private[this] def update(key: Long, value: ArrayIndexedSequence[A]): Unit = {
    if (key == 0) {
      _zeroValue = value
    } else {
      val i = seekEntryOrOpen(key)
      if (i < 0) {
        val j = i & IndexMask
        _keys(j) = key
        _values(j) = value
        _size += 1
        if ((i & VacantBit) != 0) _vacant -= 1
        else if (imbalanced) repack
      } else {
        _keys(i) = key
        _values(i) = value
      }
    }
  }

  private[this] def removeKey(key: Long): this.type = {
    assertLegalKey(key)
    if (key == 0) {
      _zeroValue = null
    } else {
      val i = seekEntry(key)
      if (i >= 0) {
        _size -= 1
        _vacant += 1
        _keys(i) = Long.MinValue
        _values(i) = null
      }
    }
    this
  }

  /** Returns the indexed sequence associated with the specified key. If the key does not exist in this map, then
    * an (unmodifiable) empty sequence is returned.
    *
    * The returned collection supports traversal and concurrent modification.
    *
    * @param key  the key to lookup
    * @return the indexed sequence associated with the specified key
    */
  def apply(key: Long): IndexedSequence[A] = {
    assertLegalKey(key)
    val values = getOrNull(key)
    if (values eq null) IndexedSequence.empty[A] else values
  }

  /** Returns **a copy of** the indexed sequence associated with the specified key. If the key does not exist in this
    * map, then an (unmodifiable) empty sequence is returned.
    *
    * The returned collection is a copy, and supports traversal and concurrent modification. As such, changes to the
    * bindings of this map will not be visible in the returned collection.
    *
    * @param key  the key to lookup
    * @return a copy of the indexed sequence associated with the specified key
    */
  def copyOf(key: Long): IndexedSequence[A] = {
    assertLegalKey(key)
    val values = getOrNull(key)
    if (values eq null) IndexedSequence.empty[A] else values.copy
  }

  /** Assigns the specified `value` to a specified `key`.
    *
    * @param key    the key to which to bind the new value
    * @param value  the value to bind to the key
    * @return       a reference to this collection
    */
  def addBinding(key: Long, value: A): this.type = {
    assertLegalKey(key)
    val multiset = getOrNull(key)
    if (multiset eq null) {
      val sequence = new ArrayIndexedSequence[A](16, makeElementData)
      sequence += value
      this(key) = sequence
    } else {
      multiset += value
    }
    this
  }

  /** Removes the binding of `value` to `key` if it exists, otherwise this operation does not have any effect.
    *
    * If `key` has no values as a result of this operation, then the indexed sequence assigned to that key will also be removed.
    *
    * @param key     the key of the binding
    * @param value   the value to remove
    * @return        a reference to this collection
    */
  def removeBinding(key: Long, value: A): this.type = {
    assertLegalKey(key)
    val multiset = getOrNull(key)
    if (multiset ne null) {
      multiset -= value
      if (multiset.isEmpty) removeKey(key) // TODO: this requires to seek entry twice!
    }
    this
  }

  /** Returns `true` if this map contains the specified key, and returns `false` otherwise.
    *
    * @param key  the key to test for containment
    * @return `true` if this map contains the specified key
    */
  def contains(key: Long): Boolean =
    if (key == Long.MinValue) false
    else if (key == 0) _zeroValue ne null
    else seekEntry(key) >= 0

  /** Returns the number of entries in this mutable map.
    *
    * @return the number of entries in this mutable map.
    */
  def size: Int = if (_zeroValue ne null) _size + 1 else _size

  /** Returns an iterator over all keys in this map.
    *
    * @return an iterator over all keys in this map.
    */
  def keysIterator: Iterator[Long] = new AbstractIterator[Long] {
    private[this] val kz = _keys
    private[this] var nextKeyPresent: Boolean = _zeroValue ne null
    private[this] var nextKey: Long = 0L
    private[this] var index = 0
    override def hasNext: Boolean = nextKeyPresent || (index < kz.length && {
      var q = kz(index)
      while (q == -q) {
        index += 1
        if (index >= kz.length) return false
        q = kz(index)
      }
      nextKey = kz(index)
      nextKeyPresent = true
      index += 1
      true
    })
    override def next(): Long = {
      if (!nextKeyPresent && !hasNext) throw new NoSuchElementException
      val ans = nextKey
      nextKeyPresent = false
      ans
    }
  }

  /** Returns a set containing all keys of this map.
    *
    * @return a set containing all keys of this map.
    */
  def keySet: Set[Long] = new AbstractSet[Long] with Set[Long] {
    override def contains(key: Long) = self.contains(key)
    override def iterator = self.keysIterator
    override def +(elem: Long): Set[Long] = Set[Long]() ++ this + elem
    override def -(elem: Long): Set[Long] = Set[Long]() ++ this - elem
    override def size = self.size
    override def foreach[B](f: Long => B) = self.keysIterator foreach f
  }

  /** Returns an iterator over all values in this map.
    *
    * @return an iterator over all values in this map.
    */
  def valuesIterator: Iterator[Iterator[A]] = new AbstractIterator[Iterator[A]] {
    private[this] val kz = _keys
    private[this] val vz = _values
    private[this] var nextValueArrayPresent: Boolean = _zeroValue ne null
    private[this] var nextValueArray: Iterator[A] = if (nextValueArrayPresent) _zeroValue.iterator else null
    private[this] var index = 0
    override def hasNext: Boolean = nextValueArrayPresent || (index < kz.length && {
      var q = kz(index)
      while (q == -q) {
        index += 1
        if (index >= kz.length) return false
        q = kz(index)
      }
      nextValueArray = vz(index).iterator
      nextValueArrayPresent = true
      index += 1
      true
    })
    override def next(): Iterator[A] = {
      if (!nextValueArrayPresent && !hasNext) throw new NoSuchElementException
      val ans = nextValueArray
      nextValueArrayPresent = false
      ans
    }
  }

  def foreach[U](f: (Long, IndexedSequence[A]) => U): Unit = {
    val length = _keys.length
    if (_zeroValue ne null) f(0, _zeroValue)
    var index = 0
    while (index < length) {
      val q = _keys(index)
      if (q == -q) {
        // Seek the next entry.
      } else {
        // index is the next entry.
        f(q, _values(index))
      }
      index += 1
    }
  }

  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    var first = true
    @inline def appendKV(key: Long, value: ArrayIndexedSequence[A]): Unit = {
      if (!first) b.append(sep)
      b.append(key)
      b.append(" -> ")
      value.addString(b, "[", sep, "]")
      first = false
    }
    val length = _keys.length
    b.append(start)
    if (_zeroValue ne null) appendKV(0, _zeroValue)
    var index = 0
    while (index < length) {
      val q = _keys(index)
      if (q == -q) {
        // Seek the next entry.
      } else {
        // index is the next entry.
        appendKV(q, _values(index))
      }
      index += 1
    }
    b.append(end)
    b
  }

  override def toString: String = addString(new StringBuilder, "LongIndexedSequenceMap[", ", ", "]").result

}
