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

/** The type of sets of taxonomy nodes.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
abstract class TaxonomyNodeSet extends CollectionMakeString {
  /** The size of this node set.
    *
    * @return the number of nodes in this set.
    */
  def size: Int
  /** Tests if this set is empty.
    *
    * @return `true` if there is no element in the set, `false` otherwise.
    */
  def isEmpty: Boolean
  /** Tests if this set is not empty.
    *
    * @return `true` if this set contains at least one element, `false` otherwise.
    */
  def nonEmpty: Boolean
  /** Tests if the specified element is contained in this set.
    *
    * @param elem  the element to test for membership.
    * @return `true` if `elem` is contained in this set, `false` otherwise.
    */
  def contains(elem: TaxonomyNode): Boolean
  /** Returns `true` if the specified a predicate holds for at least one node in this set.
    *
    * @param p  the predicate used to test nodes.
    * @return `true` if the given predicate `p` is satisfied by at least one element, otherwise `false`.
    */
  def exists(p: TaxonomyNode => Boolean): Boolean
  /** Returns `true` if the specified predicate holds for all nodes in this set.
    *
    * @param p  the predicate used to test nodes.
    * @return `true` if this node set is empty or the given predicate `p` holds for all elements, otherwise `false`.
    */
  def forall(p: TaxonomyNode => Boolean): Boolean
  /** Applies a function `f` to all elements of this set.
    *
    * @param f the function that is applied for its side-effect to every element. The result of function `f` is discarded.
    */
  def foreach[U](f: TaxonomyNode => U): Unit
  def iterator: Iterator[TaxonomyNode]
  /** Returns the hash code for this set. The hash code of this set is defined to be the sum of the hash codes of the
    * nodes in the set.
    *
    * @return the hash code for this set.
    */
  override def hashCode: Int
  /** Compares the specified object with this set for equality. Returns `true` if the specified object is also a
    * `TaxonomyNodeSet`, the two sets have the same size, and every member of the specified set is contained in this set.
    *
    * This definition ensures that the equals method works correctly across different implementations of this abstract class.
    *
    * @param o  object to be compared for equality with this set
    * @return `true` if the specified object is equal to this set
    */
  override def equals(o: Any): Boolean
  override def toString: String = {
    val builder = new StringBuilder
    addString(builder, "TaxonomyNode[", ", ", "]")
    builder.result
  }
  /** For efficiency, these sets need to be mutable. However, they are unmodifiable outside of this package.
    *
    * @param elem  the node to add to the set
    * @return `true` if the set was modified as a result of this call
    */
  protected[taxonomy] def add(elem: TaxonomyNode): Boolean
}

protected[taxonomy] object TaxonomyNodeSetImpl {
  def apply(elem: TaxonomyNode): TaxonomyNodeSetImpl = {
    val set = new TaxonomyNodeSetImpl
    set.add(elem)
    set
  }
}

protected[taxonomy] final class TaxonomyNodeSetImpl extends TaxonomyNodeSet {
  private[this] val mutableSet = new collection.mutable.HashSet[TaxonomyNode]
  override def size: Int = mutableSet.size
  override def isEmpty: Boolean = mutableSet.isEmpty
  override def nonEmpty: Boolean = mutableSet.nonEmpty
  override def contains(elem: TaxonomyNode): Boolean = mutableSet.contains(elem)
  override def exists(p: TaxonomyNode => Boolean): Boolean = mutableSet.exists(p)
  override def forall(p: TaxonomyNode => Boolean): Boolean = mutableSet.forall(p)
  override def foreach[U](f: TaxonomyNode => U): Unit = mutableSet.foreach(f)
  override def iterator: Iterator[TaxonomyNode] = mutableSet.iterator
  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
    iterator.addString(b, start, sep, end)
  override def hashCode: Int = {
    var result = 0
    mutableSet.foreach { n => result += n.hashCode }
    result
  }
  override def equals(o: Any): Boolean = o match {
    case that: TaxonomyNodeSet if this.size == that.size =>
      mutableSet.forall(that.contains)
    case _ => false
  }
  override protected[taxonomy] def add(elem: TaxonomyNode): Boolean = mutableSet.add(elem)
}
