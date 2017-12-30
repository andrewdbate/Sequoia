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

import java.lang.{Iterable => jIterable}
import java.util.{Arrays, TreeMap, Iterator => jIterator}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object TrieSearchTree {

  private class Node[A >: Null <: AnyRef] {
    var value: A = null
    // An upper bound on the maximum length of a path from the current node of a leaf.
    var pathBound: Int = 0
    val children = new TreeMap[Long, Node[A]]

    def getChild(index: Long): Node[A] =
      children.get(index)

    def setChild(index: Long, node: Node[A]): Unit =
      if (node ne null) children.put(index, node)
      else children.remove(index)

    def hasNoChild: Boolean =
      children.isEmpty

    def numChildren: Int = children.size

    def ensurePathBound(bound: Int): Unit =
      if (bound > pathBound) pathBound = bound

    def getOrderedIndexesAscending: jIterable[Long] =
      children.navigableKeySet

    def getOrderedIndexesAscendingFrom(index: Long): jIterable[Long] =
      children.navigableKeySet.tailSet(index)
  }

  private def assertIncreasing(arr: Array[Long]): Unit = {
    if (arr eq null) throw new NullPointerException
    var i = 1
    while (i < arr.length) {
      if (arr(i - 1) >= arr(i))
        throw new IllegalArgumentException(Arrays.toString(arr) + " is not strictly increasing!")
      i += 1
    }
  }

}

/** An implementation of a trie-based search tree of key-value entries.
  *
  * Keys must be strictly increasing sequences of long integers, represented as an array of longs. Each value can be of
  * any reference type, and must be non-null.
  *
  * It supports the usual operations of `put`, `putIfAbsent`, `contains`, `get`, `delete`, `keys`, size`, and `isEmpty`.
  *
  * Furthermore, given a strictly increasing sequence of longs, the trie supports testing if a key exists that is a
  * subset (either strict or non-strict) of the supplied array. It also supports collecting all values associated with a
  * key that is a non-strict superset of an array of longs.
  *
  * The `put`, `contains`, and `delete` operations take time proportional to the length of the key (in the worst case).
  * The `size` and `isEmpty` operations take constant time. Construction takes constant time.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class TrieSearchTree[A >: Null <: AnyRef] extends CollectionMakeString {
  import TrieSearchTree._

  /** The root of the trie. */
  private[this] val root = new Node[A]
  /** The number of keys in the trie. */
  private[this] var numKeys: Int = 0

  @tailrec private[this] def get(x: Node[A], key: Array[Long], d: Int): Node[A] = {
    if (x eq null) return null
    if (d == key.length) return x
    val c: Long = key(d)
    return get(x.getChild(c), key, d + 1)
  }

  /**
    * @return the node where the value associated with `key`, or `null` if no value was associated.
    */
  private[this] def getOrElseCreateNodeForKey(node: Node[A], key: Array[Long]): Node[A] = {
    var currentNode = node
    var d = 0
    while (d < key.length) {
      val c: Long = key(d)
      val parent = currentNode
      currentNode = parent.getChild(c)
      if (currentNode eq null) {
        currentNode = new Node[A]
        parent.setChild(c, currentNode)
      }
      parent.ensurePathBound(key.length - d)
      d += 1
    }
    currentNode
  }

  /**
    *
    * @return `true` if the subtree rooted at `x` is empty after the deletion.
    */
  private[this] def delete(x: Node[A], key: Array[Long], d: Int): Boolean = {
    if (x eq null) return true // TODO: eliminate recursion.
    if (d == key.length) {
      if (x.value ne null) numKeys -= 1
      x.value = null
    } else {
      val c: Long = key(d)
      // TODO: maintain the pathBound of nodes to make it an exact bound instead of an upper bound.
      val doRemoveSubtree = delete(x.getChild(c), key, d + 1)
      if (doRemoveSubtree) x.setChild(c, null)
    }
    (x.value eq null) && x.hasNoChild
  }

  /**
    *
    * @param x
    * @param pattern
    * @param patternStartIndex
    * @param previouslySkippedIndex
    * @param isStrict
    * @param isDestructive
    * @param f
    * @return `true` if the subtree rooted at `x` is to be removed according to the deletion option.
    */
  private[this] def foreachKeySubset[U](x: Node[A], pattern: Array[Long], patternStartIndex: Int, previouslySkippedIndex: Boolean, isStrict: Boolean, isDestructive: Boolean, f: A => U): Boolean = { // TODO: maintain path bound during deletion
    if (x eq null) throw new NullPointerException
    var hitValue = false
    if (x.value ne null) {
      // If partially matched pattern along path so far, return true.
      if (!isStrict || previouslySkippedIndex) {
        hitValue = true
        f(x.value)
      }
    }
    if (patternStartIndex == pattern.length) {
      // If the pattern has been exhausted, but we do not have a hit, then false.
      return isDestructive && hitValue && x.hasNoChild
    }
    var patternIndex: Int = patternStartIndex
    var nowSkippedIndex = false
    val iterator: jIterator[Long] = x.getOrderedIndexesAscendingFrom(pattern(patternIndex)).iterator
    while (iterator.hasNext) {
      val childIndex: Long = iterator.next
      while (patternIndex < pattern.length && pattern(patternIndex) < childIndex) {
        nowSkippedIndex = true
        patternIndex += 1
      }
      if (patternIndex == pattern.length) {
        return isDestructive && hitValue && x.hasNoChild
      }
      // If pattern[patternIndex] == childIndex then inspect next child.
      if (pattern(patternIndex) == childIndex) {
        val toRemove = foreachKeySubset(x.getChild(childIndex), pattern, patternIndex + 1, previouslySkippedIndex || nowSkippedIndex, isStrict, isDestructive, f)
        if (toRemove) iterator.remove
      }
    }
    return isDestructive && hitValue && x.hasNoChild
  }

  private[this] def containsKeySubset(x: Node[A], pattern: Array[Long], patternStartIndex: Int, previouslySkippedIndex: Boolean, isStrict: Boolean): Boolean = {
    // TODO: implementation is similar to collectKeySubset. Remove duplication.
    if (x eq null) throw new NullPointerException
    if (x.value ne null) {
      // If partially matched pattern along path so far, return true.
      return !isStrict || previouslySkippedIndex
    } else if (patternStartIndex == pattern.length) {
      // If the pattern has been exhausted, but we do not have a hit, then false.
      return false
    }
    var patternIndex: Int = patternStartIndex
    var nowSkippedIndex = false
    val iterator: jIterator[Long] = x.getOrderedIndexesAscendingFrom(pattern(patternIndex)).iterator
    while (iterator.hasNext) {
      val childIndex: Long = iterator.next
      while (patternIndex < pattern.length && pattern(patternIndex) < childIndex) {
        nowSkippedIndex = true
        patternIndex += 1
      }
      if (patternIndex == pattern.length) {
        return false
      }
      // If pattern[patternIndex] == childIndex then inspect next child.
      if (pattern(patternIndex) == childIndex && containsKeySubset(x.getChild(childIndex), pattern, patternIndex + 1, previouslySkippedIndex || nowSkippedIndex, isStrict)) {
        return true
      }
    }
    return false
  }

  private[this] def foreachValues[U](x: Node[A], f: A => U): Unit = {
    if (x eq null) return
    if (x.value ne null) f(x.value)
    val iterator: jIterator[Long] = x.getOrderedIndexesAscending.iterator
    while (iterator.hasNext) {
      val c: Long = iterator.next
      foreachValues(x.getChild(c), f)
    }
  }

  /**
    *
    * @param x
    * @param pattern
    * @param patternIndex
    * @param isDestructive  if values are to be removed after they are visited
    * @param f
    * @tparam U
    * @return `true` if the subtree rooted at `x` is to be removed according to the deletion option.
    */
  private[this] def foreachKeySuperset[U](x: Node[A], pattern: Array[Long], patternIndex: Int, isDestructive: Boolean, f: A => U): Boolean = { // TODO: maintain path bound during deletion
    if (x eq null) throw new NullPointerException
    val remaining = pattern.length - patternIndex
    if (remaining == 0) {
      foreachValues(x, f)
      return isDestructive
    } else if (remaining > x.pathBound) {
      return false
    } else if (remaining == x.pathBound) {
      val child: Node[A] = x.getChild(pattern(patternIndex))
      if (child eq null) return false
      val toRemove = foreachKeySuperset(child, pattern, patternIndex + 1, isDestructive, f)
      if (toRemove) {
        if (x.numChildren == 1) return true // Optimization: just delete parent if this is the only child.
        else x.setChild(pattern(patternIndex), null) // Remove only this child child.
      }
      return false
    } else {
      val iterator: jIterator[Long] = x.getOrderedIndexesAscending.iterator
      var break = false
      while (!break && iterator.hasNext) {
        val childIndex: Long = iterator.next
        if (childIndex < pattern(patternIndex)) {
          val toRemove = foreachKeySuperset(x.getChild(childIndex), pattern, patternIndex, isDestructive, f)
          if (toRemove) iterator.remove
        } else if (childIndex == pattern(patternIndex)) {
          val toRemove = foreachKeySuperset(x.getChild(childIndex), pattern, patternIndex + 1, isDestructive, f)
          if (toRemove) iterator.remove
        } else {
          break = true
        }
      }
      return x.hasNoChild
    }
  }

  /** Returns the value associated with the specified key.
    *
    * @param key  the key
    * @return the value associated with the given key if the key exists, and `null` if the key is not in the trie.
    * @throws NullPointerException if `key` is `null`
    */
  def get(key: Array[Long]): A = {
    assertIncreasing(key)
    val x: Node[A] = get(root, key, 0)
    if (x eq null) null else x.value
  }

  /** Returns `true` if this trie contains the specified key.
    *
    * @param key  the key
    * @return `true` if this trie contains `key` and `false` otherwise.
    * @throws NullPointerException if `key` is `null`
    */
  def contains(key: Array[Long]): Boolean = {
    assertIncreasing(key)
    get(key) ne null
  }

  /** Inserts the specified key-value pair into the trie, overwriting the old value with the new value if the key is
    * already present. If the key or the value is `null`, then a NullPointerException is thrown.
    *
    * @param key    the key
    * @param value  the value
    * @throws NullPointerException if `key` or `value` is `null`
    */
  def put(key: Array[Long], value: A): Unit = {
    if (value eq null) throw new NullPointerException
    assertIncreasing(key)
    val node = getOrElseCreateNodeForKey(root, key)
    val isAbsent: Boolean = node.value eq null
    if (isAbsent) numKeys += 1
    node.value = value
  }

  /** Inserts the specified key-value pair into the trie provided the key is not already present.
    * If the key or the value is `null`, then a NullPointerException is thrown.
    *
    * @param key    the key
    * @param value  the value
    * @throws NullPointerException if `key` or `value` is `null`
    */
  def putIfAbsent(key: Array[Long], value: A): Unit = {
    if (value eq null) throw new NullPointerException
    assertIncreasing(key)
    val node = getOrElseCreateNodeForKey(root, key)
    val isAbsent: Boolean = node.value eq null
    if (isAbsent) {
      numKeys += 1
      node.value = value
    }
  }

  /** If given key is already in this trie, returns the associated value.
    *
    * Otherwise, the value is computed from the given expression `op`, it is associated with the specified key in the
    * map and that value is returned.
    *
    * @param  key the key to test
    * @param  op  the computation yielding the value to associate with `key`, if `key` is previously unbound.
    * @return     the value associated with key (either previously or as a result of executing the method).
    * @throws NullPointerException if `key` or `value` is `null`
    */
  def getOrElseUpdate(key: Array[Long], op: => A): A  = {
    assertIncreasing(key)
    val node = getOrElseCreateNodeForKey(root, key)
    val existingValue: A = node.value
    val isAbsent: Boolean = existingValue eq null
    if (isAbsent) {
      // Do not evaluate value until its use.
      val theValue: A = op
      if (theValue eq null) throw new NullPointerException
      numKeys += 1
      node.value = theValue
      theValue
    } else {
      existingValue
    }
  }

  /** Returns the number of key-value pairs in this trie.
    *
    * @return the number of key-value pairs in this trie.
    */
  def size: Int = numKeys

  /** Returns `true` if this trie contains no keys, and `false` otherwise.
    *
    * @return `true` if this trie contains no keys, and `false` otherwise.
    */
  def isEmpty: Boolean = numKeys == 0

  /** Removes the key from the trie if the key is present.
    *
    * @param key  the key to remove
    * @throws NullPointerException if `key` is `null`
    */
  def delete(key: Array[Long]): Unit = {
    assertIncreasing(key)
    delete(root, key, 0)
  }

  /** Collects all keys of this trie in an iterable collection.
    *
    * @return all keys of this trie in an iterable collection.
    */
  def keys: Iterable[Seq[Long]] = {
    val results = new ArrayBuffer[Seq[Long]]
    def collect(x: Node[A], prefix: Seq[Long]): Unit = {
      if (x eq null) return
      if (x.value ne null) results += prefix
      val iterator: jIterator[Long] = x.getOrderedIndexesAscending.iterator
      while (iterator.hasNext) {
        val c: Long = iterator.next
        collect(x.getChild(c), prefix :+ c)
      }
    }
    val x: Node[A] = get(root, Array.empty[Long], 0)
    collect(x, Nil)
    results
  }

  /** Applies a function `f` to each value of this trie that is associated with a key that is a subset of the supplied
    * argument.
    *
    * @param superset  the superset for all selected keys
    * @param f the function that is applied for its side-effect to the value of each selected keys. The result of function f is discarded.
    * @tparam U
    */
  def foreachKeySubset[U](superset: Array[Long])(f: A => U): Unit = {
    assertIncreasing(superset)
    foreachKeySubset(root, superset, 0, false, isStrict = false, isDestructive = false, f)
  }

  /** Applies a function `f` to each value of this trie that is associated with a key that is a **strict** subset of the supplied
    * argument.
    *
    * @param superset  the strict superset for all selected keys
    * @param f the function that is applied for its side-effect to the value of each selected keys. The result of function f is discarded.
    * @tparam U
    */
  def foreachKeyStrictSubset[U](superset: Array[Long])(f: A => U): Unit = {
    assertIncreasing(superset)
    foreachKeySubset(root, superset, 0, false, isStrict = true, isDestructive = false, f)
  }

  /** Applies a function `f` to each value of this trie that is associated with a key that is a subset of the supplied
    * argument, and then removes that key from the trie.
    *
    * @param superset  the superset for all selected keys to be removed
    * @param f the function that is applied for its side-effect to the value of each selected keys. The result of function f is discarded.
    * @tparam U
    */
  def removeKeySubset[U](superset: Array[Long])(f: A => U): Unit = {
    assertIncreasing(superset)
    foreachKeySubset(root, superset, 0, false, isStrict = false, isDestructive = true, f)
  }

  /** Applies a function `f` to each value of this trie that is associated with a key that is a **strict** subset of the supplied
    * argument, and then removes that key from the trie.
    *
    * @param superset  the strict superset for all selected keys to be removed
    * @param f the function that is applied for its side-effect to the value of each selected keys. The result of function f is discarded.
    * @tparam U
    */
  def removeKeyStrictSubset[U](superset: Array[Long])(f: A => U): Unit = {
    assertIncreasing(superset)
    foreachKeySubset(root, superset, 0, false, isStrict = true, isDestructive = true, f)
  }

  /** Returns `true` if the trie contains a key that is a subset of the supplied argument.
    *
    * @param superset  the superset of a key to test
    */
  def containsKeySubset(superset: Array[Long]): Boolean = {
    assertIncreasing(superset)
    containsKeySubset(root, superset, 0, false, false)
  }

  /** Returns `true` if the trie contains a key that is a **strict** subset of the supplied argument.
    *
    * @param superset  the strict superset of a key to test
    */
  def containsKeyStrictSubset(superset: Array[Long]): Boolean = {
    assertIncreasing(superset)
    containsKeySubset(root, superset, 0, false, true)
  }

  /** Applies a function `f` to each value of this trie that is associated with a key that is a superset of the supplied
    * argument.
    *
    * @param subset  the subset for all selected keys
    * @param f the function that is applied for its side-effect to the value of each selected keys. The result of function f is discarded.
    * @tparam U
    */
  def foreachKeySuperset[U](subset: Array[Long])(f: A => U): Unit = {
    assertIncreasing(subset)
    foreachKeySuperset(root, subset, 0, false, f)
  }

  /** Applies a function `f` to each value of this trie that is associated with a key that is a *strict* superset of the supplied
    * argument.
    *
    * @param subset  the strict subset for all selected keys
    * @param f the function that is applied for its side-effect to the value of each selected keys. The result of function f is discarded.
    * @tparam U
    */
  def removeKeySuperset[U](subset: Array[Long])(f: A => U): Unit = {
    assertIncreasing(subset)
    foreachKeySuperset(root, subset, 0, true, f)
  }

  override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    b.append(start)
    var first = true
    for (key <- keys) {
      if (!first) b.append(sep)
      b.append(key.toString).append(" -> ").append(get(key.toArray))
      first = false
    }
    b.append(end)
    b
  }

  override def toString: String = addString(new StringBuilder, "TrieSearchTree[\n", "\n", "\n]").result

}
