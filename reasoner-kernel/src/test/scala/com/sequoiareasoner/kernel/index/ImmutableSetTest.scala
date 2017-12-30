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

import org.scalatest.FunSuite

class ImmutableSetTest extends FunSuite {

  test("Immutable empty set (implemented as special case)") {
    val set: ImmutableSet[String] = ImmutableSet.empty[String]
    assert(set.isEmpty)
    assert(set.size === 0)
    assert(!set.contains("3"))
    assert(!set("4"))
    assert(set.iterator.sameElements(Iterator.empty))
    var numElems = 0
    set foreach { _ => numElems += 1 }
    assert(numElems === 0)
    assert(set.toSeq === Nil)
    assert(set.forall{ _ => false } === true)
    assert(set.exists{ _ => true } === false)
    assert(set.filter{ _ => true}.isEmpty)
    assert(set.filter{ _ => false}.isEmpty)
  }

  test("Immutable set size 1 (implemented as special case)") {
    val set: ImmutableSet[String] = ImmutableSet("3")
    assert(set.nonEmpty)
    assert(set.size === 1)
    assert(set.contains("3"))
    assert(!set.contains("4"))
    assert(!set("4"))
    assert(set.iterator.sameElements(Iterator("3")))
    var numElems = 0
    set foreach { elem =>
      assert(elem === "3")
      numElems += 1
    }
    assert(numElems === 1)
    assert(set.toSeq === Seq("3"))
    assert(set.forall{ _ => false } === false)
    assert(set.exists{ _ => true } === true)
    assert(set.filter{ _ => true} === set)
    val filterEmpty = set.filter{ _ => false}
    assert(filterEmpty !== set)
    assert(set !== filterEmpty)
    assert(filterEmpty.isEmpty)
  }

  test("Immutable set size 2 (implemented as special case)") {
    val set: ImmutableSet[String] = ImmutableSet("3", "4")
    assert(set.nonEmpty)
    assert(set.size === 2)
    assert(!set.contains("2"))
    assert(set.contains("3"))
    assert(set.contains("4"))
    assert(!set.contains("5"))
    assert(!set("2"))
    assert(set("3"))
    assert(set("4"))
    assert(!set("5"))
    assert(set.iterator.sameElements(Iterator("3", "4")) || set.iterator.sameElements(Iterator("4", "3")))
    var numElems = 0
    var seen3 = false
    var seen4 = false
    set foreach { elem =>
      seen3 = seen3 || elem == "3"
      seen4 = seen4 || elem == "4"
      numElems += 1
    }
    assert(seen3 && seen4)
    assert(numElems === 2)
    val seq = set.toSeq
    assert(seq === Seq("3", "4") || seq === Seq("4", "3"))
    assert(set.forall{ _ => false } === false)
    assert(set.exists{ _ => true } === true)
    assert(set.filter{ _ => true} === set)
    val filterEmpty = set.filter{ _ => false}
    assert(filterEmpty !== set)
    assert(set !== filterEmpty)
    assert(filterEmpty.isEmpty)
    assert(filterEmpty === ImmutableSet.empty)
    val filter3 = set.filter{ _ == "3" }
    assert(filter3 === ImmutableSet("3"))
    assert(ImmutableSet("3") === filter3)
    assert(filter3 !== ImmutableSet.empty)
    assert(ImmutableSet.empty[String] !== filter3)
    val filter4 = set.filter{ _ == "4" }
    assert(filter4 === ImmutableSet("4"))
    assert(ImmutableSet("4") == filter4)
    assert(filter4 !== ImmutableSet.empty)
    assert(ImmutableSet.empty[String] !== filter4)
  }

  test("Immutable set size 3 or more") {
    val maximum = 10
    val elemsScalaSet: Set[String] = Set[String](0.toString, 1.toString) ++ (2 until maximum).map{ _.toString }
    val set: ImmutableSet[String] = ImmutableSet(0.toString, 1.toString, (2 until maximum).map{ _.toString }: _*)
    assert(set.nonEmpty)
    assert(set.size === maximum)
    assert(!set.contains("-1"))
    assert(!set.contains("-2"))
    for (i <- elemsScalaSet) {
      assert(set(i))
      assert(set.contains(i))
    }
    assert(set.iterator.toSet.sameElements(elemsScalaSet))
    var numElems = 0
    val seen = new collection.mutable.HashSet[String]
    set foreach { elem =>
      numElems += 1
      // Check that the element has not been seen before.
      assert(seen.add(elem))
    }
    assert(numElems === maximum)
    assert(seen.toSet === elemsScalaSet)
    val seq = set.toSeq
    assert(seq.sorted === elemsScalaSet.toSeq.sorted)
    val intRegex = """(\d+)""".r
    assert(set.forall{
      arg => arg match {
        case intRegex(_*) => true
        case _            => false
      }
    } === true)
    assert(set.exists{
      arg => arg match {
        case intRegex(_*) => false
        case _            => true
      }
    } === false)
    val filterEmpty = set.filter{ _ => false}
    assert(filterEmpty !== set)
    assert(set !== filterEmpty)
    assert(filterEmpty.isEmpty)
    assert(filterEmpty === ImmutableSet.empty)
    assert(set.filter{ _ => true} === set)
    def isEven(i: Int): Boolean = i % 2 == 0
    def isEvenString(i: String): Boolean = isEven(i.toInt)
    val filterEven: ImmutableSet[String] = set.filter(isEvenString)
    val filterEvenScalaSet: Set[String] = (0 until maximum).filter(isEven).map(_.toString).toSet
    assert(filterEven.iterator.toSet === filterEvenScalaSet)
    assert(filterEven !== ImmutableSet.empty[String])
    assert(ImmutableSet.empty[String] !== filterEven)
    def isOdd(i: Int): Boolean = i % 2 == 1
    def isOddString(i: String): Boolean = isOdd(i.toInt)
    val filterOdd = set.filter(isOddString)
    val filterOddScalaSet: Set[String] = (0 until maximum).filter(isOdd).map(_.toString).toSet
    assert(filterOdd.iterator.toSet === filterOddScalaSet)
    assert(filterOdd !== ImmutableSet.empty[String])
    assert(ImmutableSet.empty[String] !== filterOdd)
  }

}
