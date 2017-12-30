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

/** A set of integers that can be mutated.
  *
  * This purpose of this class is to provide a by-hand integer primitive type specialized mutable set implementation
  * that avoids the overhead of boxing and unboxing, backed by a fast integer array.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class IntSet extends CollectionMakeString {
  private val map: IntIntMap = IntIntMapImpl()

  /** Tests if some element is contained in this set.
    *
    * This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
    *
    * @param elem the element to test for membership.
    * @return `true` if `elem` is contained in this set, `false` otherwise.
    */
  final def apply(elem: Int): Boolean = contains(elem)

  /** Updates the presence of a single element in this set.
    *
    * This method allows one to add or remove an element `elem` from this set depending on the value of parameter
    * `included`. Typically, one would use the following syntax:
    * {{{
    *    set(elem) = true  // adds element
    *    set(elem) = false // removes element
    * }}}
    *
    * @param elem     the element to be added or removed
    * @param included a flag indicating whether element should be included or excluded.
    */
  def update(elem: Int, included: Boolean): Unit =
    if (included) this += elem else this -= elem

  /** Adds a single element to the set.
    *
    * @param elem  the element to be added.
    * @return the mutable set itself.
    */
  def +=(elem: Int): this.type = {
    map.update(elem, 0)
    this
  }

  /** Removes a single element from this mutable set.
    *
    * @param elem  the element to remove.
    * @return the mutable set itself.
    */
  def -=(elem: Int): this.type = {
    map.remove(elem)
    this
  }

  /** Tests if some element is contained in this set.
    *
    * @param elem the element to test for membership.
    * @return `true` if `elem` is contained in this set, `false` otherwise.
    */
  def contains(elem: Int): Boolean = map.contains(elem)

  /** The size of this mutable set.
    *
    * @return the number of elements in this mutable set.
    */
  def size: Int = map.size

  /** Tests whether this set is empty.
    *
    * @return `true` if the set does not contain any elements, `false` otherwise.
    */
  def isEmpty: Boolean = map.isEmpty

  /** Creates a new iterator over all elements in this set.
    *
    * @return a new iterator over all elements in this set.
    */
  def iterator: Iterator[Int] = map.keysIterator

  /** Appends all bindings of this map to a string builder using start, end, and separator strings. The written text
    * begins with the string `start` and ends with the string `end`. Inside, the string representations (w.r.t. the
    * method toString) of all elements of this traversable or iterator are separated by the string `sep`.
    *
    * @param b     the builder to which strings are appended.
    * @param start the starting string.
    * @param sep   the separator string.
    * @param end   the ending string.
    * @return      the string builder `b` to which elements were appended.
    */
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    iterator.addString(b, start, sep, end)

  /** Converts this mutable set to a string.
    *
    * @return   a string representation of this collection. By default this string consists of `IntSet` followed
    *           by all elements separated by commas and enclosed in square brackets.
    */
  override def toString = mkString("IntSet[", ", ", "]")

  /** The hash code for this set. It is defined to be the sum of the integers contained in this set.
    *
    * @return the hash code for this set.
    */
  override def hashCode: Int = {
    var res = 0
    val it = iterator
    while (it.hasNext)
      res += it.next.hashCode
    res
  }

  /** Compares the specified object with this set for equality. Returns `true` if the specified object is also an
    * `IntSet` and contains the same elements as this set.
    *
    * @param o  object to be compared for equality with this set.
    * @return `true` if the specified object is equal to this set.
    */
  override def equals(o: Any): Boolean = o match {
    case that: IntSet => this.map == that.map
    case _ => false
  }

}
