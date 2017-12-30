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

package com.sequoiareasoner.owlapi.util

import com.sequoiareasoner.arrayops._

import scala.collection.AbstractIterator

/**
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object CollectionUtils {

  /** Returns an unmodifiable immutable set from a Java set.
    *
    * @param jSet  the Java set
    * @param f     a bijection from A to B
    * @tparam A
    * @tparam B
    * @return an unmodifiable immutable set
    */
  def buildSet[A, B](jSet: java.util.Set[A])(f: A => B): Set[B] = {
    val jSetSize = jSet.size
    val elems = new Array[Any](jSetSize)
    val it = jSet.iterator
    crange (0, jSetSize) { i =>
      { elems(i) = f(it.next) }
    }
    new Set[B] {
      override def contains(elem: B): Boolean = ccontains(elems, elem)
      override def +(elem: B): Nothing = throw new UnsupportedOperationException
      override def -(elem: B): Nothing = throw new UnsupportedOperationException
      override def iterator: Iterator[B] = new AbstractIterator[B] {
        private[this] var index = 0
        override def hasNext: Boolean = index < elems.size
        override def next: B = {
          val result = elems(index).asInstanceOf[B]
          index += 1
          result
        }
      }
      override def foreach[U](f: B => U): Unit = cforeach(elems){ elem => f(elem.asInstanceOf[B]) }
      override def size: Int = jSetSize
    }
  }

  /** Returns an immutable sequence from a Java list.
    *
    * @param jList  the Java list
    * @param f      a function from A to B
    * @tparam A
    * @tparam B
    * @return an immutable list
    */
  def buildList[A, B](jList: java.util.List[A])(f: A => B): Seq[B] = {
    val jListSize = jList.size
    val elems = new Array[Any](jListSize)
    val it = jList.iterator
    crange (0, jListSize) { i =>
      { elems(i) = f(it.next) }
    }
    new collection.immutable.IndexedSeq[B] {
      override def length: Int = jListSize
      override def apply(idx: Int): B = elems(idx).asInstanceOf[B]
      override def iterator: Iterator[B] = new AbstractIterator[B] {
        private[this] var index = 0
        override def hasNext: Boolean = index < jListSize
        override def next: B = {
          val result = elems(index).asInstanceOf[B]
          index += 1
          result
        }
      }
    }
  }

}
