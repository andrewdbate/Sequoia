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

package com.sequoiareasoner.owlapi

import com.sequoiareasoner.kernel.taxonomy.{ImmutableIRISet, TaxonomyNode, TaxonomyNodeSet}
import com.sequoiareasoner.owlapi.util.WeakIdentityMap
import com.sequoiareasoner.owlapi.wrapper.OWLConverter
import org.semanticweb.owlapi.model.{OWLClass, OWLRuntimeException}
import org.semanticweb.owlapi.reasoner.{Node, NodeSet}
import java.util.{AbstractSet => jAbstractSet, Collection => jCollection, Iterator => jIterator, Set => jSet}

import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.reasoner.{SequoiaException, SequoiaRuntimeException}

/** Converter from Sequoia OWL to OWL API.
  *
  * Facade class for conversion from Sequoia objects to OWL API objects.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object TaxonomyNodeConverter {
  // This class implements wrapper classes to efficiently redirect calls to their Sequoia counterparts.
  // This also means that changes to the underlying collections are reflected in these views.

  private[this] val cache = new WeakIdentityMap[TaxonomyNode, Node[OWLClass]]

  private[this] final class OWLClassSetWrapper(peer: ImmutableIRISet) extends jAbstractSet[OWLClass] {
    override def size: Int = peer.size
    override def contains(o: Any): Boolean = o match {
      case o: OWLClass => peer.contains(OWLConverter.convert(o.getIRI))
      case _ => false
    }
    override def iterator: jIterator[OWLClass] = new jIterator[OWLClass] {
      private[this] val underlying = peer.iterator
      override def next: OWLClass = EntityConverter.toOWLClass(underlying.next)
      override def hasNext: Boolean = underlying.hasNext
    }
    override def remove(o: Any) = throw new UnsupportedOperationException
    override def removeAll(c: jCollection[_]) = throw new UnsupportedOperationException
    override def add(e: OWLClass) = throw new UnsupportedOperationException
    override def addAll(c: jCollection[_ <: OWLClass]) = throw new UnsupportedOperationException
    override def clear = throw new UnsupportedOperationException
    override def retainAll(c: jCollection[_]) = throw new UnsupportedOperationException
    // Safe to cache hashCode for performance because peer is immutable.
    private[this] var hashCodeCached: Int = 0
    override def hashCode: Int = {
      if (hashCodeCached == 0) hashCodeCached = super.hashCode
      hashCodeCached
    }
  }

  // PRECONDITION: peer.contains(exclude)
  private[this] class OWLClassSetExcludeWrapper(peer: ImmutableIRISet, exclude: IRI) extends jAbstractSet[OWLClass] {
    override def size: Int = peer.size - 1
    override def contains(o: Any): Boolean = o match {
      case o: OWLClass =>
        val iri: IRI = OWLConverter.convert(o.getIRI)
        iri != exclude && peer.contains(iri)
      case _ => false
    }
    override def iterator: jIterator[OWLClass] = new jIterator[OWLClass] {
      private[this] val underlying = peer.iterator
      private[this] var seen: Int = 0
      override def next: OWLClass = {
        val elem: IRI = underlying.next
        seen += 1
        val res = if (elem == exclude) underlying.next else elem
        EntityConverter.toOWLClass(res)
      }
      override def hasNext: Boolean = seen < size
    }
    override def remove(o: Any) = throw new UnsupportedOperationException
    override def removeAll(c: jCollection[_]) = throw new UnsupportedOperationException
    override def add(e: OWLClass) = throw new UnsupportedOperationException
    override def addAll(c: jCollection[_ <: OWLClass]) = throw new UnsupportedOperationException
    override def clear = throw new UnsupportedOperationException
    override def retainAll(c: jCollection[_]) = throw new UnsupportedOperationException
    // Safe to cache hashCode for performance because both peer and excluded value are immutable.
    private[this] var hashCodeCached: Int = 0
    override def hashCode: Int = {
      if (hashCodeCached == 0) hashCodeCached = super.hashCode
      hashCodeCached
    }
  }

  private[this] class OWLClassNodeImpl(private val peer: TaxonomyNode) extends Node[OWLClass] {
    private[this] val entities = new OWLClassSetWrapper(peer.equivalentClasses)
    override def getSize: Int = peer.equivalentClasses.size
    override def isSingleton: Boolean = peer.equivalentClasses.size == 1
    override def isBottomNode: Boolean = peer.equivalentClasses.contains(IRI.owlNothing)
    override def isTopNode: Boolean = peer.equivalentClasses.contains(IRI.owlThing)
    override def contains(entity: OWLClass): Boolean = entities.contains(entity)
    override def getRepresentativeElement: OWLClass = EntityConverter.toOWLClass(peer.equivalentClasses.representative)
    override def getEntities: jSet[OWLClass] = entities
    override def getEntitiesMinus(e: OWLClass): jSet[OWLClass] =
      if (contains(e)) new java.util.HashSet(new OWLClassSetExcludeWrapper(peer.equivalentClasses, OWLConverter.convert(e.getIRI))) else entities // WORKAROUND for Protege bug.
    override def getEntitiesMinusBottom: jSet[OWLClass] =
      if (isBottomNode) new java.util.HashSet(new OWLClassSetExcludeWrapper(peer.equivalentClasses, IRI.owlNothing)) else entities // WORKAROUND for Protege bug.
    override def getEntitiesMinusTop: jSet[OWLClass] =
      if (isTopNode) new java.util.HashSet(new OWLClassSetExcludeWrapper(peer.equivalentClasses, IRI.owlThing)) else entities // WORKAROUND for Protege bug.
    override def iterator: jIterator[OWLClass] = new jIterator[OWLClass] {
      private[this] val underlying = peer.equivalentClasses.iterator
      override def next: OWLClass = EntityConverter.toOWLClass(underlying.next)
      override def hasNext: Boolean = underlying.hasNext
    }
    override def equals(o: Any): Boolean = o match {
      case that: OWLClassNodeImpl =>
        this.peer.equivalentClasses == that.peer.equivalentClasses
      case that: Node[_] =>
        this.getSize == that.getSize && this.getEntities == that.getEntities
      case _ => false
    }
    override def hashCode: Int = entities.hashCode // For compatibility with the OWL API.
  }

  def convertClassNode(node: TaxonomyNode): Node[OWLClass] = cache.getOrElseUpdate(node, new OWLClassNodeImpl(node))

  private[this] class OWLClassNodeSetWrapper(peer: TaxonomyNodeSet) extends jAbstractSet[Node[OWLClass]] {
    override def size: Int = peer.size
    override def isEmpty: Boolean = peer.isEmpty
    override def contains(o: Any): Boolean =
      try {
        val set = o.asInstanceOf[Node[OWLClass]]
        val targetSize = set.getSize
        peer.exists( node => node.equivalentClasses.size == targetSize && {
          val it = set.iterator
          var res = true
          while (res && it.hasNext) res = node.equivalentClasses.contains(OWLConverter.convert(it.next.getIRI))
          res
        })
      } catch { case _: ClassCastException => false }
    override def iterator: jIterator[Node[OWLClass]] = new jIterator[Node[OWLClass]] {
      private[this] val underlying = peer.iterator
      override def next: Node[OWLClass] = convertClassNode(underlying.next())
      override def hasNext: Boolean = underlying.hasNext
    }
    override def add(e: Node[OWLClass]): Boolean = throw new UnsupportedOperationException
    override def clear: Unit = throw new UnsupportedOperationException
    override def addAll(c: jCollection[_ <: Node[OWLClass]]): Boolean = throw new UnsupportedOperationException
    override def retainAll(c: jCollection[_]): Boolean = throw new UnsupportedOperationException
    override def remove(o: Any): Boolean = throw new UnsupportedOperationException
    override def removeAll(c: jCollection[_]): Boolean = throw new UnsupportedOperationException
    // Safe to cache hashCode for performance because both peer will not be mutated.
    private[this] var hashCodeCached: Int = 0
    override def hashCode: Int = {
      if (hashCodeCached == 0) hashCodeCached = super.hashCode
      hashCodeCached
    }
  }

  private[this] class FlattenedOWLClassNodeSet(nodeSet: TaxonomyNodeSet) extends jAbstractSet[OWLClass] {
    override val size: Int = nodeSet.iterator.foldLeft(0){ (acc, node) => acc + node.equivalentClasses.size }
    override def isEmpty: Boolean = size == 0
    override def contains(o: Any): Boolean = o match {
      case o: OWLClass => nodeSet exists { node => node.equivalentClasses contains OWLConverter.convert(o.getIRI)}
      case _ => false
    }
    override def iterator: jIterator[OWLClass] = new jIterator[OWLClass] {
      private[this] val iterators: Iterator[TaxonomyNode] = nodeSet.iterator
      private[this] var currentIterator: Iterator[IRI] =
        if (!iterators.hasNext) Iterator.empty
        else iterators.next.equivalentClasses.iterator
      // Call this before next and hasNext to ensure that the current iterator is not exhausted.
      private[this] def updateCurrentIterator: Unit =
        while (!currentIterator.hasNext && iterators.hasNext) {
          currentIterator = iterators.next.equivalentClasses.iterator
        }
      def hasNext: Boolean = {
        updateCurrentIterator
        currentIterator.hasNext
      }
      def next: OWLClass = {
        updateCurrentIterator
        EntityConverter.toOWLClass(currentIterator.next)
      }
    }
    override def add(e: OWLClass): Boolean = throw new UnsupportedOperationException
    override def addAll(c: jCollection[_ <: OWLClass]): Boolean = throw new UnsupportedOperationException
    override def clear: Unit = throw new UnsupportedOperationException
    override def remove(o: Any): Boolean = throw new UnsupportedOperationException(s"Cannot remove $o.")
    override def removeAll(c: jCollection[_]): Boolean = throw new UnsupportedOperationException
    override def retainAll(c: jCollection[_]): Boolean = throw new UnsupportedOperationException
    // Safe to cache hashCode for performance because both peer will not be mutated.
    private[this] var hashCodeCached: Int = 0
    override def hashCode: Int = {
      if (hashCodeCached == 0) hashCodeCached = super.hashCode
      hashCodeCached
    }
  }

  private[this] class OWLClassNodeSetImpl(private val peer: TaxonomyNodeSet) extends NodeSet[OWLClass] {
    private[this] val nodes = new OWLClassNodeSetWrapper(peer)
    override def isEmpty: Boolean = peer.isEmpty
    override def isSingleton: Boolean = peer.size == 1
    override def isTopSingleton: Boolean = isSingleton && peer.exists(node => node.equivalentClasses.contains(IRI.owlThing))
    override def isBottomSingleton: Boolean = isSingleton && peer.exists(node => node.equivalentClasses.contains(IRI.owlNothing))
    override def containsEntity(e: OWLClass): Boolean = peer.exists(node => node.equivalentClasses.contains(OWLConverter.convert(e.getIRI)))
    override def getNodes: jSet[Node[OWLClass]] = nodes
    override def getFlattened: jSet[OWLClass] = new java.util.HashSet(new FlattenedOWLClassNodeSet(peer)) // WORKAROUND for Protege bug.
    override def iterator: jIterator[Node[OWLClass]] = getNodes.iterator
    override def equals(o: Any): Boolean = o match {
      case that: OWLClassNodeSetImpl => this.peer == that.peer
      case that: NodeSet[_] => nodes == that.getNodes
      case _ => false
    }
    override def hashCode: Int = nodes.hashCode // For compatibility with the OWL API.
  }

  def convertClassNodes(nodeSet: TaxonomyNodeSet): NodeSet[OWLClass] = new OWLClassNodeSetImpl(nodeSet)

  def convert(e: SequoiaException): OWLRuntimeException = ExceptionConverter.convert(e)

  def convert(e: SequoiaRuntimeException): OWLRuntimeException = ExceptionConverter.convert(e)

}