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

/** Base type for mutable indexed sequences.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @tparam A  the type of elements of this indexed sequence
  */
abstract class MutableIndexedSequence[A] extends IndexedSequence[A] {

  /** Adds a single element to this mutable indexed sequence.
    *
    * @param elem  the element to add.
    * @return the mutable indexed sequence itself.
    */
  def +=(elem: A): this.type

  /** Removes a single element from this mutable indexed sequence.
    *
    * @param elem  the element to remove.
    * @return the mutable indexed sequence itself.
    */
  def -=(elem: A): this.type

  /** Clears the contents of this mutable indexed sequence. After this operation, the collection is empty.
    */
  def clear: Unit

  /** Returns an indexed sequence containing the same elements as this collection and is such that modifications
    * to this collection are not visible in the copy. Furthermore, the original collection can be concurrently
    * modified while the returned collection is traversed.
    *
    * @return a copy of this mutable indexed sequence.
    */
  def copy: MutableIndexedSequence[A]

}
