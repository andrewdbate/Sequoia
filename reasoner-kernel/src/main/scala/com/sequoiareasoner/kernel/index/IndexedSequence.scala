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

/** Base type for mutable and immutable indexed sequences.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @tparam A  the type of elements of this indexed sequence
  */
abstract class IndexedSequence[A] extends CollectionMakeString {
  def apply(index: Int): A
  def length: Int
  /** Returns `true` if this list contains no elements.
    *
    * @return `true` if this list contains no elements.
    */
  def isEmpty: Boolean
  /** Returns `true` if this list contains at least one element.
    *
    * @return `true` if this list contains at least one element.
    */
  def nonEmpty: Boolean
  def iterator: Iterator[A]
  def forall(p: A => Boolean): Boolean
  def exists(p: A => Boolean): Boolean
  /** Returns `true` if this indexed sequence contains the specified element. More formally, returns `true` if and only
    * if this sequence contains at least one element `e` such that `o == e`.
    *
    * @param o element whose presence in this list is to be tested.
    * @return `true` if this list contains the specified element.
    */
  def contains(o: A): Boolean
  def foreach[U](f: A => U): Unit
  /** Returns the hash code for the indexed sequence. The hash code of an indexed sequence is defined to be the result
    * of the following calculation:
    * {{{
    *   var result = 1
    *   for (elem <- this) { result = 31*result + (if (elem == null) 0 else elem.hashCode) }
    * }}}

    * @return the hash code for the indexed sequence.
    */
  override def hashCode: Int
  /** Returns `true` if the specified object is also an indexed sequence and both contain the same elements in the same
    * order. The implementation of equals must work correctly across different implementations of this class.
    *
    * @param o  the object to be compared for equality
    * @return `true` if the specified object is equal to this indexed sequence.
    */
  override def equals(o: Any): Boolean
}

object IndexedSequence {

  private[this] object EmptyIndexedSequence extends IndexedSequence[Any] {
    override def apply(index: Int): Any = throw new IndexOutOfBoundsException
    override def length: Int = 0
    override def isEmpty: Boolean = true
    override def nonEmpty: Boolean = false
    override def iterator: Iterator[Any] = Iterator.empty
    override def forall(p: Any => Boolean): Boolean = true
    override def exists(p: Any => Boolean): Boolean = false
    override def contains(o: Any): Boolean = false
    override def foreach[U](f: Any => U): Unit = {}
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      b.append(start).append(end)
    override def hashCode: Int = 1
    override def equals(o: Any): Boolean = o match {
      case that: IndexedSequence[_] => that.isEmpty
      case _ => false
    }
  }

  // Empty indexed sequence.
  def empty[A] = EmptyIndexedSequence.asInstanceOf[IndexedSequence[A]]

  // Singleton indexed sequence.
  def apply[A](elem: A): IndexedSequence[A] = new IndexedSequence[A] {
    override def apply(index: Int): A =
      if (index == 0) elem
      else throw new IndexOutOfBoundsException
    override def length: Int = 1
    override def isEmpty: Boolean = false
    override def nonEmpty: Boolean = true
    override def iterator: Iterator[A] = Iterator(elem)
    override def forall(p: A => Boolean): Boolean = p(elem)
    override def exists(p: A => Boolean): Boolean = p(elem)
    override def contains(o: A): Boolean = elem == o
    override def foreach[U](f: A => U): Unit = f(elem)
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      b.append(start).append(elem).append(end)
    override def hashCode: Int = if (elem == null) 31 else 31*elem.hashCode
    override def equals(o: Any): Boolean = o match {
      case that: IndexedSequence[_] => that.length == 1 && that(0) == elem
      case _ => false
    }
  }

  // Indexed sequence length two.
  def apply[A](elem1: A, elem2: A): IndexedSequence[A] = new IndexedSequence[A] {
    override def apply(index: Int): A =
      if (index == 0) elem1
      else if (index == 1) elem2
      else throw new IndexOutOfBoundsException
    override def length: Int = 2
    override def isEmpty: Boolean = false
    override def nonEmpty: Boolean = true
    override def iterator: Iterator[A] = Iterator(elem1, elem2)
    override def forall(p: A => Boolean): Boolean = p(elem1) && p(elem2)
    override def exists(p: A => Boolean): Boolean = p(elem1) || p(elem2)
    override def contains(o: A): Boolean = elem1 == o || elem2 == o
    override def foreach[U](f: A => U): Unit = { f(elem1); f(elem2) }
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      b.append(start).append(elem1).append(sep).append(elem2).append(end)
    override def hashCode: Int = 31*(31 + (if (elem1 == null) 0 else elem1.hashCode)) + (if (elem2 == null) 0 else elem2.hashCode)
    override def equals(o: Any): Boolean = o match {
      case that: IndexedSequence[_] => that.length == 2 && that(0) == elem1 && that(1) == elem2
      case _ => false
    }
  }

}
