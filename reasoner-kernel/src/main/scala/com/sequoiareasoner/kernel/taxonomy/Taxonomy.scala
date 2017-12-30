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

package com.sequoiareasoner.kernel.taxonomy

import com.sequoiareasoner.kernel.index.{IndexedSequence, TotalIRIMultiMap}
import com.sequoiareasoner.kernel.owl.iri.IRI

import scala.annotation.tailrec
import scala.collection.mutable

object Taxonomy {

  /** An optimized representation for immutable sets of size 1.
    *
    * @param elem  the UID of the IRI.
    */
  private[this] final class SingletonImmutableIRISet(elem: Int) extends ImmutableIRISet {
    override def representative: IRI = new IRI(elem)
    override def contains(elem: IRI): Boolean = this.elem == elem.uid
    override def size: Int = 1
    override def isEmpty: Boolean = false
    override def iterator: Iterator[IRI] = Iterator(new IRI(elem))
    override def hashCode: Int = elem
    override def equals(that: Any): Boolean = that match {
      case that: ImmutableIRISet => that.size == 1 && that.contains(new IRI(elem))
      case _ => false
    }
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      b ++= start ++= new IRI(elem).toString ++= end
  }

  /** An immutable IRI set implemented using an array of IRI UIDs to avoid boxing.
    * The array passed to the constructor will be sorted in place and is assumed to *not contain repetitions*.
    *
    * @param elems  the array of IRI UIDs.
    */
  private[this] final class ArrayBackedImmutableIRISet(private val elems: Array[Int]) extends ImmutableIRISet {
    java.util.Arrays.sort(elems)
    override def representative: IRI =
      if (elems.length == 0) throw new NoSuchElementException
      else new IRI(elems(0))
    override def contains(elem: IRI): Boolean = {
      @tailrec def binarySearch(from: Int, to: Int): Boolean =
        if (to == from) false else {
          val idx = from + (to - from - 1) / 2
          val cmp = elem.uid - elems(idx)
          if (cmp < 0) {
            binarySearch(from, idx)
          } else if (cmp > 0) {
            binarySearch(idx + 1, to)
          } else {
            true
          }
        }
      binarySearch(0, elems.length)
    }
    override def size: Int = elems.length
    override def isEmpty: Boolean = elems.length > 0
    override def iterator: Iterator[IRI] = elems.iterator map { uid => new IRI(uid) } // TODO: remove boxing
    override def hashCode: Int = {
      @tailrec def sum(acc: Int, idx: Int): Int =
        if (idx == elems.length) acc
        else sum(acc + elems(idx), idx + 1)
      sum(0, 0)
    }
    override def equals(that: Any): Boolean = that match {
      case that: ArrayBackedImmutableIRISet =>
        // Optimized case when we know that arrays can be compared directly.
        java.util.Arrays.equals(this.elems, that.elems)
      case that: ImmutableIRISet =>
        var canBeEqual = this.size == that.size
        var i = 0
        while (canBeEqual && i < elems.length) {
          canBeEqual = that.contains(new IRI(elems(i)))
          i += 1
        }
        canBeEqual
      case _ =>
        false
    }
    override def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder =
      iterator.addString(b, start, sep, end)
  }

  /** A fresh TaxonomyNode containing an OWLClass that does not occur in a taxonomy.
    * Such nodes are returned to queries when FreshEntityPolicy is set to ALLOW.
    */
  private final class FreshTaxonomyNode(member: IRI, taxonomy: Taxonomy) extends TaxonomyNode {
    require(member.isImported)
    override def equivalentClasses: ImmutableIRISet = new SingletonImmutableIRISet(member.uid)
    override def directSuperNodes = TaxonomyNodeSetImpl(taxonomy.topNode)
    override def allSuperNodes = TaxonomyNodeSetImpl(taxonomy.topNode)
    override def directSubNodes = TaxonomyNodeSetImpl(taxonomy.bottomNode)
    override def allSubNodes = TaxonomyNodeSetImpl(taxonomy.bottomNode)
  }

  /** Stores information about a OWLClass or the purpose of classification. It is read-only for
    * public access but provides package-private ways of modifying it.
    *
    * @constructor Constructing the class node for a given set of equivalent classes.
    *
    * @param equivalentClasses  the set of equivalent $OWLClass objects
    */
  private[this] final class ClassNode(override val equivalentClasses: ImmutableIRISet) extends TaxonomyNode {
    /** OWLClass nodes, except for the bottom node, whose members are direct sub-classes of the members of this node. */
    override val directSubNodes: TaxonomyNodeSet = new TaxonomyNodeSetImpl
    /** OWLClass nodes whose members are direct super-classes of the members of this node. */
    override val directSuperNodes: TaxonomyNodeSet = new TaxonomyNodeSetImpl
    private[this] def allNodes(sub: Boolean): TaxonomyNodeSet = {
      val result = new TaxonomyNodeSetImpl
      val todo = new mutable.ArrayStack[TaxonomyNode]
      val init = if (sub) directSubNodes else directSuperNodes
      init foreach { n => todo += n }
      while (todo.nonEmpty) {
        val next = todo.pop
        if (result add next) {
          val toPush = if (sub) next.directSubNodes else next.directSuperNodes
          toPush foreach { n => todo += n }
        }
      }
      result
    }
    override def allSubNodes: TaxonomyNodeSet = allNodes(sub = true)
    override def allSuperNodes: TaxonomyNodeSet = allNodes(sub = false)
  }

  // equivalents must not contain repetitions.
  private[this] def createImmutableIRISet(equivalents: Seq[Int]): ImmutableIRISet =
    if (equivalents.size == 1) new SingletonImmutableIRISet(equivalents.head)
    else new ArrayBackedImmutableIRISet(equivalents.toArray)

  /** `superConcepts` must contain the transitive closure of the subclass relationships in the taxonomy.
    * However, reflexivity is not required, nor are edges of the form A -> owl:Thing or owl:Nothing -> A.
    *
    * @param superConcepts the map containing the subclass relationships.
    * @return the transitively reduced taxonomy.
    */
  def apply(superConcepts: TotalIRIMultiMap[IRI]): Taxonomy = {
    // The map from class IRIs to class nodes (LongMap is significantly faster than HashMap).
    val classNodes = new mutable.LongMap[TaxonomyNode]
    // The class IRIs that are equivalent to owl:Nothing.
    val nothingEquivalents = new mutable.ListBuffer[Int]
    nothingEquivalents += IRI.owlNothing.uid
    // The class IRIs that are equivalent to owl:Thing.
    val thingEquivalents = new mutable.ListBuffer[Int]
    thingEquivalents += IRI.owlThing.uid

    // directSupers(a) returns all the direct super concepts of a.
    val directSupers = superConcepts.copy

    // The set of IRI UIDs that have a subclass.
    val hasSubClass = new mutable.BitSet

    superConcepts.foreachKeys { conceptA =>
      assert(conceptA.isImported)
      if (conceptA.isThing) {
        directSupers.foreach(conceptA) { conceptB => thingEquivalents += conceptB.uid }
        directSupers.removeKey(conceptA)
      } else if (directSupers(conceptA) contains IRI.owlNothing) {
        // conceptA is equivalent to owl:Nothing.
        nothingEquivalents += conceptA.uid
        directSupers.removeKey(conceptA)
        superConcepts.removeKey(conceptA)
      } else {
        // equivalents maintains all the concepts equivalent to conceptA (we do not need to check for repetitions).
        val equivalents = new mutable.ListBuffer[Int]
        equivalents += conceptA.uid
        // Only consider directSupers which is needed to correctly handle cycles.
        directSupers.foreach(conceptA){ conceptB =>
          if (conceptA == conceptB) {
            directSupers.removeBinding(conceptA, conceptB)
            superConcepts.removeBinding(conceptA, conceptB)
          } else if (superConcepts(conceptB) contains conceptA) {
            // conceptA and conceptB are equivalent.
            equivalents += conceptB.uid
            directSupers.removeKey(conceptB)
            superConcepts.removeKey(conceptB)
            directSupers.removeBinding(conceptA, conceptB)
            superConcepts.removeBinding(conceptA, conceptB)
            superConcepts.removeKey(conceptB)
          } else {
            hasSubClass += conceptB.uid
            superConcepts.foreach(conceptB) { conceptC =>
              if (conceptB != conceptC) directSupers.removeBinding(conceptA, conceptC)
            }
          }
        }
        val nodeForA = new ClassNode(createImmutableIRISet(equivalents))
        for (iriUID <- equivalents) classNodes.put(iriUID, nodeForA)
      }
    }

    val nothingImmutableEquivalents: ImmutableIRISet = createImmutableIRISet(nothingEquivalents)
    val nothingNode = new ClassNode(nothingImmutableEquivalents)
    for (iriUID <- nothingEquivalents) classNodes.put(iriUID, nothingNode)

    val thingImmutableEquivalents: ImmutableIRISet = createImmutableIRISet(thingEquivalents)
    val thingNode = new ClassNode(thingImmutableEquivalents)
    for (iriUID <- thingEquivalents) classNodes.put(iriUID, thingNode)

    directSupers foreachKeys { a =>
      val subNode: TaxonomyNode = classNodes(a.uid)
      // Ensure that trivial subclass relationships are in the taxonomy.
      if (!hasSubClass.contains(a.uid)) {
        nothingNode.directSuperNodes.add(subNode)
        subNode.directSubNodes.add(nothingNode)
      }
      val supers: IndexedSequence[IRI] = directSupers(a)
      if (supers.isEmpty) {
        // If this node does not have any super concepts, then it only has the trivial superclass owl:Thing.
        subNode.directSuperNodes.add(thingNode)
        thingNode.directSubNodes.add(subNode)
      } else {
        supers foreach { sup =>
          val superNode: TaxonomyNode = classNodes.getOrElseUpdate(sup.uid, {
            val newNode = new ClassNode(new SingletonImmutableIRISet(sup.uid))
            // If this new super node does not exist as a key, it only has the trivial superclass owl:Thing.
            newNode.directSuperNodes.add(thingNode)
            thingNode.directSubNodes.add(newNode)
            newNode
          })
          superNode.directSubNodes.add(subNode)
          subNode.directSuperNodes.add(superNode)
        }
      }
    }

    if (nothingNode.directSuperNodes.isEmpty) {
      assert(thingNode.directSubNodes.isEmpty)
      nothingNode.directSuperNodes.add(thingNode)
      thingNode.directSubNodes.add(nothingNode)
    }

    new Taxonomy(classNodes, IRI.owlNothing, IRI.owlThing)
  }

}

/** A taxonomy in the form of a lattice of TaxonomyNodes. For each OWLClass, the taxonomy holds a TaxonomyNode object
  * from which direct sub- and super-nodes can be retrieved.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param classNodes  the map from class UIDs of IRIs to class nodes.
  * @param bottomIRI   the IRI corresponding to the top node.
  * @param topIRI      the IRI corresponding to the bottom node.
  */
class Taxonomy private (classNodes: mutable.LongMap[TaxonomyNode], bottomIRI: IRI, topIRI: IRI) {

  /** Returns the [[TaxonomyNode]] containing the given $OWLClass as a member for the imported IRI.
    *
    * @param iri                 the IRI of the class for which to return the [[TaxonomyNode]].
    * @param allowFreshEntities  `true` if fresh entities are allowed.
    * @return the [[TaxonomyNode]] for the specified input class IRI.
    * @throws scala.NoSuchElementException if no such class IRI occurs in the taxonomy and fresh entities are disallowed.
    */
  def node(iri: IRI, allowFreshEntities: Boolean = false): TaxonomyNode = {
    require(iri.isImported)
    val node: TaxonomyNode = classNodes.getOrNull(iri.uid)
    if (node eq null) {
      if (allowFreshEntities) new Taxonomy.FreshTaxonomyNode(iri, this)
      else throw new NoSuchElementException
    } else {
      node
    }
  }

  /**
    * @return the node of this taxonomy that has no child nodes.
    */
  val bottomNode: TaxonomyNode = node(bottomIRI)

  /**
    * @return the node of this taxonomy that has no parent nodes.
    */
  val topNode: TaxonomyNode = node(topIRI)

  def allNodes: Iterable[TaxonomyNode] = classNodes.values

  /** Applies a pair of functions to the equivalent classes and subclass relationships respectively.
    *
    * @param f1  the function to invoke for each set of equivalent classes
    * @param f2  the function to invoke for each pair of IRIs in a subclass relationship
    */
  def foreach[U1, U2](f1: ImmutableIRISet => U1, f2: (IRI, IRI) => U2): Unit = {
    val toProcess = new mutable.Queue[TaxonomyNode]
    toProcess += bottomNode
    val visited = new mutable.HashSet[TaxonomyNode]
    while (toProcess.nonEmpty) {
      val current = toProcess.dequeue
      if (visited add current) {
        val equivalentClasses: ImmutableIRISet = current.equivalentClasses
        f1(equivalentClasses)
        val currentRepresentative: IRI = equivalentClasses.representative
        val directSuperNodes = current.directSuperNodes
        for (n <- directSuperNodes)
          f2(currentRepresentative, n.equivalentClasses.representative)
        directSuperNodes foreach { elem => toProcess += elem }
      }
    }
  }

  /** Returns a pretty printed string of the taxonomy.
    *
    * @return a pretty printed string of the taxonomy.
    */
  override def toString: String = {
    val builder = new StringBuilder
    def f1(equivalentClasses: ImmutableIRISet): Unit =
      if (equivalentClasses.size > 1)
        equivalentClasses.addString(builder, "EquivalentClasses(", " ", ")\n")
    def f2(sub: IRI, sup: IRI): Unit =
      if (sub != bottomIRI && sup != topIRI) { // Skip trivial subsumptions.
        builder.append("SubClassOf(")
        builder.append(sub)
        builder.append(' ')
        builder.append(sup)
        builder.append(')')
        builder.append('\n')
      }
    foreach(f1, f2)
    builder.result
  }

}
