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

package com.sequoiareasoner.kernel.taxonomy

import com.sequoiareasoner.kernel.index.CollectionMakeString
import com.sequoiareasoner.kernel.owl.iri.IRI

/** An immutable set of IRIs.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
abstract class ImmutableIRISet extends CollectionMakeString {
  /** Returns the representative of this set if it is non-empty, otherwise throws [[NoSuchElementException]].
    * If two sets are equal, then they must have the same representative.
    *
    * @return the representative of this set if it is non-empty.
    */
  def representative: IRI
  /** Tests if the specified IRI is contained in this set.
    *
    * @param iri the IRI to test for membership.
    * @return `true` if `iri` is contained in this set, `false` otherwise.
    */
  def contains(iri: IRI): Boolean
  /** Returns the number of IRIs in this immutable set.
    *
    * @return the number of IRIs in this immutable set.
    */
  def size: Int
  /** Tests whether this set is empty.
    *
    * @return `true` if the set does not contain any IRIs, `false` otherwise.
    */
  def isEmpty: Boolean
  /** Returns a new iterator over all IRIs in this set.
    *
    * @return a new iterator over all IRIs in this set.
    */
  def iterator: Iterator[IRI]
  /** The hash code for this set. It is defined to be the sum of the UIDs of the IRIs contained in this set.
    *
    * @return the hash code for this set.
    */
  override def hashCode: Int
  /** Compares the specified object with this set for equality. Returns `true` if the specified object is also an
    * `ImmutableIRISet` and contains the same IRIs as this set.
    *
    * @param o  object to be compared for equality with this set.
    * @return `true` if the specified object is equal to this set.
    */
  override def equals(o: Any): Boolean

  override def toString: String = {
    val builder = new StringBuilder
    addString(builder, "ImmutableIRISet[", ", ", "]")
    builder.result
  }
}
