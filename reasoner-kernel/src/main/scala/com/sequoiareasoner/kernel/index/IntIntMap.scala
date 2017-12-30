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

/** A base class for maps of integer keys and integer values that can be mutated.
  * This purpose of this trait is to provide by-hand integer primitive type specialized mutable map implementations
  * that avoid the overhead of boxing and unboxing, possibly backed by fast integer arrays.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
abstract class IntIntMap extends CollectionMakeString {

  /** Retrieves the value which is associated with the given key. If there is no mapping from the given key to a value,
    * this method throws a `NoSuchElementException`.
    *
    * @param  key the key
    * @return the value associated with the given key, or throws a `NoSuchElementException` if none exists.
    */
  def apply(key: Int): Int

  /** Adds a new key/value pair to this map. If the map already contains a mapping for the key, it will be overridden by
    * the new value.
    *
    * @param key   the key to update
    * @param value the new value
    */
  def update(key: Int, value: Int): Unit

  /** If given key is present in this map and the associated value is greater than the new value, then it will be
    * overridden by the new value. If given key is not present in this map, then a new key/value pair is added to this
    * map. Otherwise, the map is unmodified.
    *
    * @param key    the key to check if present
    * @param value  the new value
    * @return `true` if this map was modified as a result of this method
    */
  def compareAndUpdate(key: Int, value: Int): Boolean

  /** If given key is present in this map, returns associated value.
    * Otherwise, adds a new key/value pair to this map and returns that value.
    *
    * @param  key    the key to check if present
    * @param  value  the value to associate with `key`, if `key` is previously unbound
    * @return the value associated with key (either previously or as a result of executing the method)
    */
  def getOrElseUpdate(key: Int, value: Int): Int

  /** If given key is present in this map, then the associated value is incremented by `1`.
    * Otherwise, maps the key to `1`.
    *
    * This method has the same behaviour as the following, but is more efficient.
    * {{{
    *   update(key, getOrElseUpdate(key, 0) + 1)
    * }}}
    *
    * @param  key    the key whose associated value is to be incremented by `1`.
    */
  def increment(key: Int): Unit

  /** If given key is present in this map, then the associated value is decremented by `1`.
    * Otherwise, maps the key `-1`.
    *
    * This method has the same behaviour as the following, but is more efficient.
    * {{{
    *   update(key, getOrElseUpdate(key, 0) - 1)
    * }}}
    *
    * @param  key    the key whose associated value is to be decremented by `1`.
    */
  def decrement(key: Int): Unit

  /** Optionally returns the value associated with a key.
    *
    * @param  key  the key value
    * @return an option value containing the value associated with `key` in this map, or `None` if none exists.
    */
  def get(key: Int): Option[Int]

  /** Removes a key from this map.
    *
    * @param key the key to be removed
    * @return the map itself.
    */
  def remove(key: Int): this.type

  /** Tests whether this map contains a binding for a key.
    *
    * @param key the key
    * @return `true` if there is a binding for `key` in this map, `false` otherwise
    */
  def contains(key: Int): Boolean

  /** Returns the value associated with a key, or a default value if the key is not contained in the map.
    *
    * @param  key      the key
    * @param  default  a default value in case no binding for `key` is found in the map
    * @return the value associated with `key` if it exists, otherwise the `default` value
    */
  def getOrElse(key: Int, default: Int): Int

  /** The size of this mutable hash map.
    *
    * @return the number of elements in this mutable hash map.
    */
  def size: Int

  /** Tests whether this map is empty.
    *
    * @return `true` if the map does not contain any key/value binding, `false` otherwise.
    */
  def isEmpty: Boolean

  /** Returns a new iterator over all key/value pairs of this map.
    *
    * @return the new iterator.
    */
  def iterator: Iterator[(Int, Int)]

  /** Returns an iterator for all keys.
    *
    * @return an iterator over all keys.
    */
  def keysIterator: Iterator[Int]

  /** Appends all bindings of this map to a string builder using start, end, and separator strings. The written text
    * begins with the string `start` and ends with the string `end`. Inside, the string representations of all bindings
    * of this map in the form of `key -> value` are separated by the string `sep`.
    *
    * @param b     the builder to which strings are appended.
    * @param start the starting string.
    * @param sep   the separator string.
    * @param end   the ending string.
    * @return      the string builder `b` to which elements were appended.
    */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    iterator.map { case (k, v) => k + " -> " + v }.addString(b, start, sep, end)

  /** Converts this mutable map to a string.
    *
    * @return   a string representation of this collection. By default this string consists of `IntIntMap` followed
    *           by all elements separated by commas and enclosed in square brackets.
    */
  override def toString = mkString("IntIntMap[", ", ", "]")

  /** Returns the hash code for this map. The hash code of the map is defined to be the sum of all keys plus the sum
    * of all values.
    *
    * @return the hash code for this map.
    */
  override def hashCode: Int

  /** Returns `true` if the specified object is also an integer-integer map and the two maps contain the same mappings.
    * The implementation of equals must work correctly across different implementations of this class.
    *
    * @param o  the object to be compared for equality
    * @return `true` if the specified object is equal to this integer-integer map.
    */
  override def equals(o: Any): Boolean

}
