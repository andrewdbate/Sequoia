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

import scala.collection.mutable
import scala.util.Random

class IntIntIntMapTest extends FunSuite {

  private[this] val fillFactors = Array[Float](0.25f, 0.5f, 0.75f, 0.9f, 0.99f)

  def testPut(fillFactor: Float) {
    test (s"test put [fillFactor = $fillFactor]") {
      val map: IntIntMap = IntIntMapImpl(100, fillFactor)
      for (i <- 0 until 100000) {
        assert(!map.contains(i))
        map.update(i, i)
        assert(i + 1 === map.size)
        assert(i === map(i))
      }
      for (i <- 0 until 100000) {
        assert(i === map(i))
      }
    }
  }

   def testPutNegative(fillFactor: Float) {
     test (s"test put negative [fillFactor = $fillFactor]") {
       val map: IntIntMap = IntIntMapImpl(100, fillFactor)
       for (i <- 0 until 100000) {
         map.update(-i, -i)
         assert(i + 1 === map.size)
         assert(-i === map(-i))
       }
       for (i <- 0 until 100000) {
         assert(-i === map(-i))
       }
     }
  }


  def testPutRandom(fillFactor: Float) {
    test (s"test put random [fillFactor = $fillFactor]") {
      val size = 100 * 1000
      val set = new mutable.HashSet[Int]
      val values = new Array[Int](size)
      while (set.size < size) set += Random.nextInt
      var i = 0
      for (v: Int <- set) {
        values(i) = v
        i += 1
      }
      val map: IntIntMap = IntIntMapImpl(100, fillFactor)
      for (i <- 0 until values.length) {
        assert(!map.contains(values(i)))
        map.update(values(i), values(i))
        assert(i + 1 === map.size)
        assert(values(i) === map(values(i)))
      }
      for (i <- 0 until values.length) {
        assert(values(i) === map(values(i)))
      }
    }
  }

  def testRemove(fillFactor: Float) {
    test (s"test remove [fillFactor = $fillFactor]") {
      val map: IntIntMap = IntIntMapImpl(100, fillFactor)
      var addCnt = 0
      var removeCnt = 0
      for (i <- 0 until 100000) {
        assert(!map.contains(addCnt))
        map.update(addCnt, addCnt)
        addCnt += 1
        assert(!map.contains(addCnt))
        map.update(addCnt, addCnt)
        addCnt += 1
        assert(removeCnt === map(removeCnt))
        map.remove(removeCnt)
        removeCnt += 1
        assert(i + 1 === map.size) // Map grows by one element on each iteration.
      }
      for (i <- removeCnt until addCnt) {
        assert(i === map(i))
      }
    }
  }

  for (ff: Float <- fillFactors) {
    testPut(ff)
    testPutNegative(ff)
    testPutRandom(ff)
    testRemove(ff)
  }

  test ("test iterator") {
    val scalaMap = new mutable.HashMap[Int, Int]
    val primitiveMap = IntIntMapImpl()
    for (_ <- 0 until 100000) {
      val (k, v) = (Random.nextInt, Random.nextInt)
      scalaMap.update(k, v)
      primitiveMap.update(k, v)
    }
    assert(scalaMap.iterator.toSet === primitiveMap.iterator.toSet)
  }

}
