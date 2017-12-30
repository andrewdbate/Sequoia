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

import java.lang.ref.ReferenceQueue
import java.lang.ref.WeakReference

import scala.collection.mutable

object WeakIdentityMap {

  /** A key that maintains a weak reference to an object which should be compared by object identity.
    *
    * @tparam K  the type of the referenced object
    *
    * @constructor Creates an instance of this class and registers it with the specified reference queue.
    * @param    key the referenced object
    * @param    queue the reference queue
    */
  private final class IdentityWeakReference[K <: AnyRef] private[WeakIdentityMap](key: K, queue: ReferenceQueue[K]) extends WeakReference[K](key, queue) {

    /**
      * Returns {@code true} if the argument is an instance of {@link Key} that refers to the same object.
      *
      * @param obj the object to compare
      * @return `true` if the argument is an instance of `IdentityWeakReference` that refers to the same object, otherwise `false`.
      */
    override def equals(obj: Any): Boolean = obj match {
      case that: IdentityWeakReference[_] =>
        if (this eq that) return true
        val k1: K = this.get
        val k2: AnyRef = that.get
        return (k1 ne null) && (k2 ne null) && (k1 eq k2)
      case _ => false


    }

    /** Returns the identity hash code of the referenced object.
      *
      * @return the identity hash code of the referenced object.
      */
    override val hashCode: Int = System.identityHashCode(key)

  }

  /** Returns an `IdentityWeakReference`, or `null` if `key` is `null`.
    *
    * @tparam K the type of the referenced object
    * @param key the referenced object
    * @param queue the reference queue or `null`
    * @return the new instance or `null`.
    */
  private def createKey[K <: AnyRef](key: K, queue: ReferenceQueue[K] = null): IdentityWeakReference[K] =
    if (key == null) null
    else new IdentityWeakReference[K](key, queue)

}

/** A map that with weak keys that are compared for identity.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @tparam K  the key type
  * @tparam V  the value type
  */
class WeakIdentityMap[K <: AnyRef, V <: AnyRef] {
  import WeakIdentityMap._

  /** The underlying map. */
  private[this] val map = new mutable.AnyRefMap[IdentityWeakReference[K], V]()

  /** The reference queue, for detecting weak references removals. */
  private[this] val queue = new ReferenceQueue[K]()

  /** Associates the specified value with the specified key in this map. If the map previously contained a mapping for
    * this key, the old value is replaced.
    *
    * @param key  key with which the specified value is to be associated.
    * @param value  value to be associated with the specified key.
    * @return an option value containing the value associated with the key before the `put` operation, or
    *         `None` if there was no mapping for `key`.
    */
  def put(key: K, value: V): Option[V] = map.put(createKey(key, queue), value)

  /** If given key is already in this map, returns associated value.
    *
    * Otherwise, computes value from given expression `op`, stores with key in map and returns that value.
    *
    * @param  key the key to test
    * @param  defaultValue  the computation yielding the value to associate with `key`, if `key` is previously unbound.
    * @return the value associated with key (either previously or as a result of executing the method).
    */
  def getOrElseUpdate(key: K, defaultValue: => V): V = map.getOrElseUpdate(createKey(key, queue), defaultValue)

  /** Returns `true` if the map contains the specified key.
    *
    * @param key  the key whose presence in this map is to be tested
    * @return `true` if there is a mapping for `key`; `false` otherwise.
    */
  def contains(key: K): Boolean = map.contains(createKey(key))

  /** Optionally returns the value associated with a key.
    *
    * @param key  the key value
    * @return an option value containing the value associated with `key` in this map, or `None` if none exists.
    */
  def get(key: K): Option[V] = map.get(createKey(key))

  /** Removes the association for the given key, optionally returning the value previously associated with that key.
    *
    * @param key  the key
    * @return an option value containing the value previously associated with `key`, or `None` if `key` is not found.
    */
  def remove(key: K): Option[V] = map.remove(createKey(key))

  /** Returns an iterable collection containing all values currently held in this map.
    *
    * @return the values of this map as an iterable.
    */
  def values: Iterable[V] = map.values
 
  /** Removes all associations from this map. */
  def clear: Unit = map.clear
 
  /** Removes all entries from the map whose keys have been determined to be no longer referenced. */
  def removeStaleEntries: Unit =
    while (true) {
      val k = queue.poll.asInstanceOf[IdentityWeakReference[K]]
      if (k eq null) return
      map.remove(k)
    }

}