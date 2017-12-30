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

import scala.annotation.tailrec
import scala.util.Sorting

/** Base type for immutable sets of elements of a reference type.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @tparam A
  */
abstract class ImmutableSet[A <: AnyRef] extends (A => Boolean) with CollectionMakeString {
  def contains[B >: A](elem: B): Boolean
  override def apply(elem: A): Boolean = contains(elem)
  def size: Int
  def isEmpty: Boolean
  final def nonEmpty: Boolean = !isEmpty
  def iterator: Iterator[A]
  def foreach(f: A => Unit): Unit
  def toSeq: Seq[A]
  def forall(p: A => Boolean): Boolean
  def exists(p: A => Boolean): Boolean
  def filter(p: A => Boolean): ImmutableSet[A]
  def intersect(that: ImmutableSet[A]): ImmutableSet[A]
  def +(elem: A): ImmutableSet[A]
  override def hashCode: Int
  override def equals(o: Any): Boolean
  override def toString: String = {
    val builder = new StringBuilder
    addString(builder, "ImmutableSet[", ", ", "]")
    builder.result
  }
}

object ImmutableSet {
  /** An optimized representation for immutable empty sets. */
  private[this] object EmptySet extends ImmutableSet[AnyRef] {
    override def contains[B >: AnyRef](key: B): Boolean = false
    override def forall(p: AnyRef => Boolean): Boolean = true
    override def exists(p: AnyRef => Boolean): Boolean = false
    override def filter(p: AnyRef => Boolean): this.type = this
    override def toSeq: Seq[AnyRef] = Nil
    override def size: Int = 0
    override def iterator: Iterator[AnyRef] = Iterator.empty
    override def isEmpty: Boolean = true
    override def foreach(f: AnyRef => Unit): Unit = {}
    override def intersect(that: ImmutableSet[AnyRef]): this.type = this
    override def +(elem: AnyRef) = new ImmutableSingletonSet(elem)
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = b ++= start ++= end
    override def hashCode: Int = 0
    override def equals(that: Any) = that match {
      case that: ImmutableSet[_] => that.size == 0
      case _ => false
    }
  }
  /** An optimized representation for immutable sets of size 1. */
  private final class ImmutableSingletonSet[A <: AnyRef](elem: A) extends ImmutableSet[A] {
    override def contains[B >: A](elem: B): Boolean = elem == this.elem
    override def size: Int = 1
    override def isEmpty: Boolean = false
    override def iterator: Iterator[A] = Iterator(elem)
    override def foreach(f: A => Unit): Unit = f(elem)
    override def toSeq: Seq[A] = Seq(elem)
    override def forall(p: A => Boolean): Boolean = p(elem)
    override def exists(p: A => Boolean): Boolean = p(elem)
    override def filter(p: A => Boolean): ImmutableSet[A] = if (p(elem)) this else EmptySet.asInstanceOf[ImmutableSet[A]]
    override def intersect(that: ImmutableSet[A]): ImmutableSet[A] = filter(that)
    override def +(elem: A) = if (elem == this.elem) this else new ImmutableUnorderedPairSet(this.elem, elem)
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      b ++= start ++= elem.toString ++= end
    override def hashCode: Int = elem.hashCode
    override def equals(that: Any) = that match {
      case that: ImmutableSet[_] => that.size == 1 && that.contains(elem)
      case _ => false
    }
  }
  /** An optimized representation for immutable sets of size 2 */
  private final class ImmutableUnorderedPairSet[A <: AnyRef](elem1: A, elem2: A) extends ImmutableSet[A] {
    override def contains[B >: A](elem: B): Boolean = elem == elem1 || elem == elem2
    override def size: Int = 2
    override def isEmpty: Boolean = false
    override def iterator: Iterator[A] = Iterator(elem1, elem2)
    override def foreach(f: A => Unit): Unit = { f(elem1); f(elem2) }
    override def toSeq: Seq[A] = Seq(elem1, elem2)
    override def forall(p: A => Boolean): Boolean = p(elem1) && p(elem2)
    override def exists(p: A => Boolean): Boolean = p(elem1) || p(elem2)
    override def filter(p: A => Boolean): ImmutableSet[A] = {
      val pElem1 = p(elem1); val pElem2 = p(elem2)
      if (pElem1 && pElem2) this
      else if (pElem1) new ImmutableSingletonSet(elem1)
      else if (pElem2) new ImmutableSingletonSet(elem2)
      else EmptySet.asInstanceOf[ImmutableSet[A]]
    }
    override def intersect(that: ImmutableSet[A]): ImmutableSet[A] = filter(that)
    override def +(elem: A) =
      if (contains(elem)) this else new ImmutableArrayHashSet(Array[AnyRef](elem1, elem2, elem))
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      b ++= start ++= elem1.toString ++= sep ++= elem2.toString ++= end
    override def hashCode: Int = elem1.hashCode + elem2.hashCode
    override def equals(that: Any) = that match {
      case that: ImmutableSet[_] => that.size == 2 && that.contains(elem1) && that.contains(elem2)
      case _ => false
    }
  }
  /** An ordering for sorting objects based on their hash code. */
  private[this] object HashCodeOrdering extends Ordering[AnyRef] {
    override def compare(x: AnyRef, y: AnyRef): Int =
      x.hashCode - y.hashCode
  }
  /** A representation for immutable sets of size 3 or more. */
  private final class ImmutableArrayHashSet[A <: AnyRef](backingArray: Array[AnyRef]) extends ImmutableSet[A] {
    self =>
    Sorting.quickSort(backingArray)(HashCodeOrdering)
    override def contains[B >: A](elem: B): Boolean = {
      @inline @tailrec def scan(startIdx: Int, goLeft: Boolean): Boolean =
        backingArray(startIdx) == elem || {
          val nextIdx = if (goLeft) startIdx - 1 else startIdx + 1
          val inBounds = if (goLeft) nextIdx >= 0 else nextIdx < backingArray.length
          inBounds && backingArray(nextIdx).hashCode == elem.hashCode && scan(nextIdx, goLeft)
        }
      @tailrec def binarySearch(from: Int, to: Int): Boolean = {
        if (to == from) false else {
          val idx = from + (to - from - 1) / 2
          val cmp = elem.hashCode - backingArray(idx).hashCode
          if (cmp < 0) {
            binarySearch(from, idx)
          } else if (cmp > 0) {
            binarySearch(idx + 1, to)
          } else {
            // Found hashCode hit; now try to find equal element.
            scan(idx, goLeft = true) || scan(idx, goLeft = false)
          }
        }
      }
      binarySearch(0, backingArray.length)
    }
    override def size: Int = backingArray.length
    override def isEmpty: Boolean = backingArray.length == 0
    override def iterator: Iterator[A] = backingArray.asInstanceOf[Array[A]].iterator
    override def foreach(f: A => Unit): Unit = {
      @inline @tailrec def walk(idx: Int): Unit =
        if (idx < backingArray.length) { f(backingArray(idx).asInstanceOf[A]); walk(idx + 1) }
      walk(0)
    }
    override def toSeq: Seq[A] = new collection.immutable.Seq[A] {
      override def length: Int = self.size
      override def apply(idx: Int): A = self.backingArray(idx).asInstanceOf[A]
      override def iterator: Iterator[A] = self.iterator
    }
    override def forall(p: A => Boolean): Boolean = {
      @inline @tailrec def walk(idx: Int): Boolean =
        if (idx == backingArray.length) true
        else p(backingArray(idx).asInstanceOf[A]) && walk(idx + 1)
      walk(0)
    }
    override def exists(p: A => Boolean): Boolean = {
      @inline @tailrec def walk(idx: Int): Boolean =
        if (idx == backingArray.length) false
        else p(backingArray(idx).asInstanceOf[A]) || walk(idx + 1)
      walk(0)
    }
    override def filter(p: A => Boolean): ImmutableSet[A] = {
      val length = backingArray.length
      val newBackingArray = new Array[AnyRef](length)
      var w = 0 // Index of next write.
      var r = 0 // Index of next read.
      while (r < length) {
        val elem: A = backingArray(r).asInstanceOf[A]
        if (p(elem)) {
          newBackingArray(w) = elem
          w += 1
        }
        r += 1
      }
      val numWrites = w // Total number of elements written.
      numWrites match {
        case 0 => EmptySet.asInstanceOf[ImmutableSet[A]]
        case 1 => new ImmutableSingletonSet(newBackingArray(0).asInstanceOf[A])
        case 2 => new ImmutableUnorderedPairSet(newBackingArray(0).asInstanceOf[A], newBackingArray(1).asInstanceOf[A])
        case `length` => this
        case _ =>
          val finalBackingArray = new Array[AnyRef](numWrites)
          System.arraycopy(newBackingArray, 0, finalBackingArray, 0, numWrites)
          new ImmutableArrayHashSet(finalBackingArray)
      }
    }
    override def intersect(that: ImmutableSet[A]): ImmutableSet[A] =
      if (this.size <= that.size) this.filter(that) else that.filter(this)
    override def +(elem: A) =
      if (contains(elem)) this else {
        val newBackingArray = new Array[AnyRef](backingArray.length + 1)
        System.arraycopy(backingArray, 0, newBackingArray, 0, backingArray.length)
        newBackingArray(backingArray.length) = elem
        new ImmutableArrayHashSet(newBackingArray)
      }
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      iterator.addString(b, start, sep, end) // TODO: faster implementation since this names the context proc.
    override def hashCode: Int = {
      @inline @tailrec def sum(idx: Int, acc: Int): Int =
        if (idx < backingArray.length) sum(idx + 1, acc + backingArray(idx).hashCode) else acc
      sum(0, 0)
    }
    override def equals(that: Any) = that match {
      case that: ImmutableSet[A] => this.size == that.size && forall(that)
      case _ => false
    }
  }

  def empty[A <: AnyRef]: ImmutableSet[A] = EmptySet.asInstanceOf[ImmutableSet[A]]

  def apply[A <: AnyRef](elem: A): ImmutableSet[A] = new ImmutableSingletonSet(elem)

  def apply[A <: AnyRef](elem1: A, elem2: A): ImmutableSet[A] =
    if (elem1 == elem2) new ImmutableSingletonSet(elem1)
    else new ImmutableUnorderedPairSet(elem1, elem2)

  def apply[A <: AnyRef](elem1: A, elem2: A, elems: A*): ImmutableSet[A] = {
    val mutableSet = new collection.mutable.HashSet[AnyRef]
    mutableSet += elem1
    mutableSet += elem2
    mutableSet ++= elems.iterator
    new ImmutableArrayHashSet(mutableSet.toArray)
  }

  def apply[A <: AnyRef](elems: IndexedSequence[A]): ImmutableSet[A] = {
    elems.length match {
      case 0 => empty
      case 1 => apply(elems(0))
      case 2 => apply(elems(0), elems(1))
      case _ =>
        val mutableSet = new collection.mutable.HashSet[AnyRef]
        mutableSet ++= elems.iterator
        new ImmutableArrayHashSet(mutableSet.toArray)
    }
  }

}
