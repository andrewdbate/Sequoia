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

class TrieSearchTreeTest extends FunSuite {

  private[this] def collectKeySuperset(trie: TrieSearchTree[String], key: Array[Long]): List[String] = {
    // Keys and values should be returned in ascending order of the corresponding key.
    val results = new collection.mutable.ArrayBuffer[String]
    trie.foreachKeySuperset(key)( str => results.append(str) )
    results.toList
  }

  test("Basic test") {
    val trie = new TrieSearchTree[String]()
    trie.put(Array(0, 3), "0 3")
    trie.put(Array(1, 2), "1 2")
    trie.put(Array(1, 4), "1 4")
    trie.put(Array(2, 4), "2 4")
    trie.put(Array(3), "3")
    trie.put(Array(99), "99")
    trie.put(Array(100), "100")
    assert(trie.keys === Iterable(Seq(0, 3), Seq(1, 2), Seq(1, 4), Seq(2, 4), Seq(3), Seq(99), Seq(100)))
    assert(trie.containsKeySubset(Array.empty[Long]) === false)
    assert(trie.containsKeySubset(Array(1, 2, 3, 4)) === true)
    assert(collectKeySuperset(trie, Array(2)) === List("1 2", "2 4"))
  }

  test("Empty test") {
    val trie = new TrieSearchTree[String]()
    assert(trie.containsKeySubset(Array()) === false)
    trie.put(Array.empty, "")
    assert(trie.containsKeySubset(Array()) === true)
  }

  test("Subset test") {
    val trie = new TrieSearchTree[String]()
    trie.put(Array(2147483649L), "A")
    trie.put(Array(541165879296L), "B")
    trie.put(Array(678604832768L), "C")
    trie.put(Array(687194767360L), "D")
    trie.put(Array(1460288880640L), "E")
    trie.put(Array(4611686134391504897L), "F")
    assert(trie.containsKeySubset(Array(15032385537L, 678604832768L)))
  }

  test("Repeated additions test") {
    val key1 = 2147483649L
    val key2 = 541165879296L
    val key3 = 4611686134391504897L
    val trie = new TrieSearchTree[String]()
    trie.put(Array(key1), "A")
    assert(trie.get(Array(key1)) === "A")
    trie.put(Array(key2), "B")
    assert(trie.get(Array(key1)) === "A")
    trie.put(Array(key1), "C")
    assert(trie.get(Array(key1)) === "C")
    trie.put(Array(key2), "D")
    trie.put(Array(key1), "E")
    assert(trie.get(Array(key1)) === "E")
    trie.put(Array(key3), "F")
    assert(trie.get(Array(key1)) === "E")
  }

  test("putIfAbsent") {
    val key1 = 2147483649L
    val key2 = 541165879296L
    val key3 = 4611686134391504897L
    val trie = new TrieSearchTree[String]()
    trie.putIfAbsent(Array(key1), "A")
    assert(trie.get(Array(key1)) === "A")
    trie.putIfAbsent(Array(key2), "B")
    assert(trie.get(Array(key1)) === "A")
    trie.putIfAbsent(Array(key1), "C")
    assert(trie.get(Array(key1)) === "A")
    trie.putIfAbsent(Array(key2), "D")
    trie.putIfAbsent(Array(key1), "E")
    assert(trie.get(Array(key1)) === "A")
    trie.putIfAbsent(Array(key3), "F")
    assert(trie.get(Array(key1)) === "A")
  }

  test("foreachKeySuperset") {
    val trie = new TrieSearchTree[String]()
    trie.put(Array(1, 2, 3), "123")
    trie.put(Array(1, 2), "12")
    // Keys and values should be returned in ascending order of the corresponding key.
    assert(collectKeySuperset(trie, Array(2)) === List("12", "123"))
    assert(collectKeySuperset(trie, Array(1)) === List("12", "123"))
    assert(collectKeySuperset(trie, Array(3)) === List("123"))
    trie.put(Array(1, 4), "14")
    assert(collectKeySuperset(trie, Array(2)) === List("12", "123"))
    assert(collectKeySuperset(trie, Array(1)) === List("12", "123", "14"))
    assert(collectKeySuperset(trie, Array(3)) === List("123"))
    trie.put(Array(27, 99), "9927")
    trie.put(Array(0), "9927")
    assert(collectKeySuperset(trie, Array(2)) === List("12", "123"))
    assert(collectKeySuperset(trie, Array(1)) === List("12", "123", "14"))
    assert(collectKeySuperset(trie, Array(3)) === List("123"))
  }

}
