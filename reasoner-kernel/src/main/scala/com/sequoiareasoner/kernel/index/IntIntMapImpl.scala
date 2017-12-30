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
 * This is free and unencumbered software released into the public domain.
 *
 * Anyone is free to copy, modify, publish, use, compile, sell, or
 * distribute this software, either in source code form or as a compiled
 * binary, for any purpose, commercial or non-commercial, and by any
 * means.
 *
 * In jurisdictions that recognize copyright laws, the author or authors
 * of this software dedicate any and all copyright interest in the
 * software to the public domain. We make this dedication for the benefit
 * of the public at large and to the detriment of our heirs and
 * successors. We intend this dedication to be an overt act of
 * relinquishment in perpetuity of all present and future rights to this
 * software under copyright law.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * For more information, please refer to <http://unlicense.org>
 */

package com.sequoiareasoner.kernel.index

import scala.annotation.switch

object IntIntMapImpl {

  private final val FREE_KEY = 0

  /** The golden ratio; an arbitrary Int value. */
  private final val INT_PHI = 0x9E3779B9

  private final val KEEP_IF_PRESENT = 0
  private final val OVERWRITE_IF_PRESENT = 1
  private final val OVERWRITE_IF_GREATER = 2
  private final val INCREMENT_IF_PRESENT = 3
  private final val DECREMENT_IF_PRESENT = 4

  private final val OVERWRITE_IF_GREATER_UNMODIFIED = 0
  private final val OVERWRITE_IF_GREATER_MODIFIED = 1

  /** Return the least power of two greater than or equal to the specified value.
    *
    * Note that this function will return 1 when the argument is 0.
    *
    * @param x a long integer smaller than or equal to `2^62`.
    * @return the least power of two greater than or equal to the specified value.
    */
  private[this] def nextPowerOfTwo(x: Long): Long = {
    if (x == 0) return 1
    var z = x - 1
    z |= z >> 1
    z |= z >> 2
    z |= z >> 4
    z |= z >> 8
    z |= z >> 16
    (z | z >> 32) + 1
  }

  /** Returns the least power of two smaller than or equal to `2^30` and larger than or equal to `math.ceil(expected/f)`.
    *
    * @param expected the expected number of elements in a hash table.
    * @param f        the load factor.
    * @return the minimum possible size for a backing array.
    * @throws IllegalArgumentException if the necessary size is larger than `2^30`
    */
  protected def arraySize(expected: Int, f: Float): Int = {
    val s: Long = Math.max(2, nextPowerOfTwo(math.ceil(expected / f).toLong))
    if (s > (1 << 30))
      throw new IllegalArgumentException("Too large (" + expected + " expected elements with load factor " + f + ")")
    s.toInt
  }

  protected def phiMix(x: Int): Int = {
    val h: Int = x * INT_PHI
    h ^ (h >> 16)
  }

  /**
    *
    * @param expectedSize
    * @param fillFactor  the fill factor, must be between (0 and 1)
    */
  def apply(expectedSize: Int = 16, fillFactor: Float = 0.75f): IntIntMapImpl = {
    if (fillFactor <= 0 || fillFactor >= 1)
      throw new IllegalArgumentException("FillFactor must be in (0, 1).")
    if (expectedSize <= 0)
      throw new IllegalArgumentException("Size must be positive!")
    val capacity: Int = arraySize(expectedSize, fillFactor)
    new IntIntMapImpl(hasFreeKey = false,
                      freeValue = 0,
                      _size = 0,
                      mask = capacity - 1,
                      mask2 = (capacity << 1) - 1,
                      data = new Array[Int](capacity << 1),
                      fillFactor,
                      threshold = (capacity * fillFactor).toInt)
  }

}

/**
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param hasFreeKey  `true` if the map contains the special 'free' key
  * @param freeValue   the value of the 'free' key, if the 'free' key is present
  * @param _size       the number of entries in the map
  * @param mask        the mask to calculate the original position
  * @param mask2       the mask to calculate the original position
  * @param data        the single array containing oth keys and values
  * @param threshold   once the map reaches this size, it will be resized
  */
class IntIntMapImpl private (private[this] var hasFreeKey: Boolean, // TODO: reduce functional dependencies in argument values
                             private[this] var freeValue: Int,
                             private[this] var _size: Int,
                             private[this] var mask: Int,
                             private[this] var mask2: Int,
                             private[this] var data: Array[Int],
                             private[this] val fillFactor: Float,
                             private[this] var threshold: Int) extends IntIntMap {
  import IntIntMapImpl._

  private def shiftKeys(_pos: Int): Int = {
    // Shift entries with the same hash.
    var k = -1
    var pos = _pos
    val data: Array[Int] = this.data
    do {
      val last = pos
      var slot = -1
      do {
        // go to the next entry
        pos = (pos + 2) & mask2
        k = data(pos)
        if (k == FREE_KEY) {
          data(last) = FREE_KEY
          return last
        }
        // Calculate the starting slot for the current key.
        slot = (phiMix(k) & mask) << 1
      } while ((last < slot && slot <= pos) || (pos < last && last < slot) || (slot <= pos && pos < last))
      data(last) = k
      data(last + 1) = data(pos + 1)
    } while (true)
    throw new Error("Unreachable")
  }

  private[this] def rehash(newCapacity: Int): Unit = {
    threshold = ((newCapacity >> 1) * fillFactor).toInt
    mask = (newCapacity >> 1) - 1
    mask2 = newCapacity - 1

    val oldCapacity: Int = data.length
    val oldData: Array[Int] = data

    data = new Array[Int](newCapacity)
    _size = if (hasFreeKey) 1 else 0

    var i = 0
    while (i < oldCapacity) {
      val oldKey: Int = oldData(i)
      if (oldKey != FREE_KEY)
        update(oldKey, oldData(i + 1))
      i += 2
    }
  }

  private[this] def put(key: Int, value: Int, mode: Int): Int = {
    if (key == FREE_KEY) {
      (mode: @switch) match {
        case KEEP_IF_PRESENT =>
          if (hasFreeKey) {
            return freeValue
          } else {
            _size += 1
            hasFreeKey = true
            freeValue = value
            return value
          }
        case OVERWRITE_IF_PRESENT =>
          if (!hasFreeKey) _size += 1
          hasFreeKey = true
          freeValue = value
          return value
        case OVERWRITE_IF_GREATER =>
          if (hasFreeKey) {
            if (freeValue > value) {
              freeValue = value
              return OVERWRITE_IF_GREATER_MODIFIED
            } else {
              return OVERWRITE_IF_GREATER_UNMODIFIED
            }
          } else {
            _size += 1
            freeValue = value
            hasFreeKey = true
            return OVERWRITE_IF_GREATER_MODIFIED
          }
        case INCREMENT_IF_PRESENT =>
          if (hasFreeKey) {
            freeValue = freeValue + 1
          } else {
            _size += 1
            hasFreeKey = true
            freeValue = +1
          }
          return value
        case DECREMENT_IF_PRESENT =>
          if (hasFreeKey) {
            freeValue = freeValue - 1
          } else {
            _size += 1
            hasFreeKey = true
            freeValue = -1
          }
          return value
      }
    }
    var ptr: Int = (phiMix(key) & mask) << 1
    do {
      val k: Int = data(ptr)
      if (k == FREE_KEY) { // End of chain.
        val valueIndex = ptr + 1
        data(ptr) = key
        val result = (mode: @switch) match {
          case KEEP_IF_PRESENT | OVERWRITE_IF_PRESENT =>
            data(valueIndex) = value
            value
          case OVERWRITE_IF_GREATER =>
            data(valueIndex) = value
            OVERWRITE_IF_GREATER_MODIFIED
          case INCREMENT_IF_PRESENT =>
            data(valueIndex) = +1
            +1
          case DECREMENT_IF_PRESENT =>
            data(valueIndex) = -1
            -1
        }
        if (_size >= threshold) {
          // Size is set inside.
          rehash(data.length << 1)
        } else {
          _size += 1
        }
        return result
      } else if (k == key) { // Must check FREE_KEY prior to this.
        val valueIndex = ptr + 1
        (mode: @switch) match {
          case KEEP_IF_PRESENT =>
            return data(valueIndex)
          case OVERWRITE_IF_PRESENT =>
            data(valueIndex) = value
            return value
          case OVERWRITE_IF_GREATER =>
            if (data(valueIndex) > value) {
              data(valueIndex) = value
              return OVERWRITE_IF_GREATER_MODIFIED
            } else {
              return OVERWRITE_IF_GREATER_UNMODIFIED
            }
          case INCREMENT_IF_PRESENT =>
            data(valueIndex) += 1
            return value
          case DECREMENT_IF_PRESENT =>
            data(valueIndex) -= 1
            return value
        }
      }
      ptr = (ptr + 2) & mask2 // Next index.
    } while (true)
    throw new Error("Unreachable")
  }

  override def apply(key: Int): Int = {
    var ptr: Int = (phiMix(key) & mask) << 1
    if (key == FREE_KEY) {
      if (hasFreeKey) return freeValue
      else throw new NoSuchElementException
    }
    do {
      val k: Int = data(ptr)
      if (k == FREE_KEY) { // End of chain.
        throw new NoSuchElementException
      } else if (k == key) { // Must check FREE_KEY prior to this.
        return data(ptr + 1)
      }
      ptr = (ptr + 2) & mask2 // Next index.
    } while (true)
    throw new Error("Unreachable")
  }

  override def get(key: Int): Option[Int] = {
    var ptr: Int = (phiMix(key) & mask) << 1
    if (key == FREE_KEY) {
      if (hasFreeKey) return Some(freeValue)
      else return None
    }
    do {
      val k: Int = data(ptr)
      if (k == FREE_KEY) { // End of chain.
        return None
      } else if (k == key) { // Must check FREE_KEY prior to this.
        return Some(data(ptr + 1))
      }
      ptr = (ptr + 2) & mask2 // Next index.
    } while (true)
    throw new Error("Unreachable")
  }

  override def contains(key: Int): Boolean = {
    var ptr: Int = (phiMix(key) & mask) << 1
    if (key == FREE_KEY) return hasFreeKey
    do {
      val k: Int = data(ptr)
      if (k == FREE_KEY) { // End of chain.
        return false
      } else if (k == key) { // Must check FREE_KEY prior to this.
        return true
      }
      ptr = (ptr + 2) & mask2 // Next index.
    } while (true)
    throw new Error("Unreachable")
  }

  override def update(key: Int, value: Int): Unit = put(key, value, OVERWRITE_IF_PRESENT)

  def compareAndUpdate(key: Int, value: Int): Boolean = put(key, value, OVERWRITE_IF_GREATER) == OVERWRITE_IF_GREATER_MODIFIED

  override def getOrElseUpdate(key: Int, value: Int): Int = put(key, value, KEEP_IF_PRESENT)

  override def increment(key: Int): Unit = put(key, 0, INCREMENT_IF_PRESENT)

  override def decrement(key: Int): Unit = put(key, 0, DECREMENT_IF_PRESENT)

  override def getOrElse(key: Int, value: Int): Int = {
    var ptr: Int = (phiMix(key) & mask) << 1
    if (key == FREE_KEY)
      return if (hasFreeKey) freeValue else value
    do {
      val k: Int = data(ptr)
      if (k == FREE_KEY) { // End of chain.
        return value
      } else if (k == key) { // Must check FREE_KEY prior to this.
        return data(ptr + 1)
      }
      ptr = (ptr + 2) & mask2 // Next index.
    } while (true)
    throw new Error("Unreachable")
  }

  override def remove(key: Int): this.type = {
    if (key == FREE_KEY) {
      if (!hasFreeKey)
        return this
      hasFreeKey = false
      _size -= 1
      return this
    }
    var ptr: Int = (phiMix(key) & mask) << 1
    do {
      val k: Int = data(ptr)
      if (k == key) { // End of chain.
        shiftKeys(ptr)
        _size -= 1
        return this
      } else if (k == FREE_KEY) { // Must check FREE_KEY prior to this.
        return this
      }
      ptr = (ptr + 2) & mask2 // Next index.
    } while (true)
    throw new Error("Unreachable")
  }

  override def size: Int = _size

  override def isEmpty: Boolean = _size == 0

  override def iterator: Iterator[(Int, Int)] = {
    // This implementation materialises the collection on construction.
    val values = new Array[(Int, Int)](_size)
    var i = 0
    if (hasFreeKey) {
      values(i) = (0, freeValue)
      i += 1
    }
    var ptr = 2
    while (i < _size) {
      val k: Int = data(ptr)
      if (k != FREE_KEY) {
        values(i) = (k, data(ptr + 1))
        i += 1
      }
      ptr = (ptr + 2) & mask2 // Next index.
    }
    values.iterator
  }

  override def keysIterator: Iterator[Int] = {
    // This implementation materialises the collection on construction.
    val values = new Array[Int](_size)
    var i = 0
    if (hasFreeKey) {
      values(i) = 0
      i += 1
    }
    var ptr = 2
    while (i < _size) {
      val k: Int = data(ptr)
      if (k != FREE_KEY) {
        values(i) = k
        i += 1
      }
      ptr = (ptr + 2) & mask2 // Next index.
    }
    values.iterator
  }

  override def hashCode: Int = {
    var result = 0
    var i = 0
    if (hasFreeKey) {
      result += freeValue // Corresponds to key zero.
      i += 1
    }
    var ptr = 2
    while (i < _size) {
      val k: Int = data(ptr)
      if (k != FREE_KEY) {
        result += k + data(ptr + 1)
        i += 1
      }
      ptr = (ptr + 2) & mask2 // Next index.
    }
    result
  }

  override def equals(o: Any): Boolean = o match {
    case that: IntIntMap if this.size == that.size =>
      @inline def thatContainsEntry(key: Int, expectedValue: Int): Boolean = {
        val safeDefaultValue = if (expectedValue == -1) -2 else -1
        that.getOrElse(key, safeDefaultValue) == expectedValue
      }
      var i = 0
      if (hasFreeKey) {
        // Corresponds to key zero.
        if (!thatContainsEntry(0, freeValue)) return false
        i += 1
      }
      var ptr = 2
      while (i < _size) {
        val k: Int = data(ptr)
        if (k != FREE_KEY) {
          if (!thatContainsEntry(k, data(ptr + 1))) return false
          i += 1
        }
        ptr = (ptr + 2) & mask2 // Next index.
      }
      return true
    case _ => false
  }

}
