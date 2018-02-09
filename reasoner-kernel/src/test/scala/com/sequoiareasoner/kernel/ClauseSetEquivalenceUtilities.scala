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

package com.sequoiareasoner.kernel

import org.scalatest.Assertions
import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.owl.model.Axiom
import com.sequoiareasoner.kernel.structural.{DLOntology, UnsupportedFeatureObserverThrowException}

import scala.collection.mutable
import scala.language.postfixOps
import scala.reflect.ClassTag

/** Collection of utilities used to determine if two sets of clauses are equivalent up to variable renaming, functional
  * symbol renaming, and temporary IRI renaming.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object ClauseSetEquivalenceUtilities {
  import Term.x

  /** Implementation of an one-one function. */
  private[this] sealed abstract class OneOneMap[A](mapName: String) {
    type Self <: OneOneMap[A]
    protected[this] def empty: Self
    private val map = new collection.mutable.HashMap[A, A] {
      override val stringPrefix: String = mapName
    }
    def copy: Self = {
      val newMap = empty
      newMap.map ++= this.map
      newMap
    }
    def canMap(v1: A, v2: A): Boolean = map.get(v1) match {
      case Some(existing) => existing == v2
      case None =>
        if (map.values.toSet.contains(v2)) {
          false
        } else {
          map += v1 -> v2
          true
        }
    }
  }

  /** Maps equivalent temporary predicate names. */
  private[this] final class AuxIriMap extends OneOneMap[Int]("AuxIriMap") {
    override type Self = AuxIriMap
    override protected[this] def empty: Self = new AuxIriMap
  }

  /** Maps equivalent variables. */
  private[this] final class VariableMap extends OneOneMap[Term]("VariableMap") {
    override type Self = VariableMap
    override protected[this] def empty: Self = new VariableMap
  }

  /** Maps equivalent function symbols. */
  private[this] final class FunctionSymbolMap extends OneOneMap[Term]("FunctionSymbolMap") {
    override type Self = FunctionSymbolMap
    override protected[this] def empty: Self = new FunctionSymbolMap
  }

  private[this] def neighbourVars(v1: Term, v2: Term): Boolean =
    v1.isNeighbourVariable && v2.isNeighbourVariable

  private[this] def functionTerms(v1: Term, v2: Term): Boolean =
    v1.isFunctionTerm && v2.isFunctionTerm

  private[this] def canMapTerms(varMap: VariableMap, funMap: FunctionSymbolMap)(t1: Term, t2: Term): Boolean = {
    if (neighbourVars(t1, t2))
      varMap.canMap(t1, t2)
    else if (functionTerms(t1, t2))
      funMap.canMap(t1, t2)
    else
      t1 == t2
  }

  private[this] def literalsAreEquivalent[L <: Literal](idMap: AuxIriMap,
                                                        varMap: VariableMap,
                                                        funMap: FunctionSymbolMap)(l: L, r: L): Boolean = (l, r) match {
    case (Concept(iri1, t1), Concept(iri2, t2)) if iri1.isInternal && iri2.isInternal =>
      idMap.canMap(iri1.uid, iri2.uid) && canMapTerms(varMap, funMap)(t1,t2)
    case (Role(iri1, `x`, v1), Role(iri2, `x`, v2)) if iri1.isInternal && iri2.isInternal =>
      idMap.canMap(iri1.uid, iri2.uid) && canMapTerms(varMap, funMap)(v1,v2)
    case (Role(iri1, v1, `x`), Role(iri2, v2, `x`)) if iri1.isInternal && iri2.isInternal =>
      idMap.canMap(iri1.uid, iri2.uid) && canMapTerms(varMap, funMap)(v1,v2)
    case (Concept(iri1, v1), Concept(iri2, v2)) if iri1 == iri2 =>
      canMapTerms(varMap, funMap)(v1,v2)
    case (Role(iri1, `x`, v1), Role(iri2, `x`, v2)) if iri1 == iri2 =>
      canMapTerms(varMap, funMap)(v1,v2)
    case (Role(iri1, v1, `x`), Role(iri2, v2, `x`)) if iri1 == iri2 =>
      canMapTerms(varMap, funMap)(v1,v2)
    case (Equality(s1, t1), Equality(s2, t2)) =>
      canMapTerms(varMap, funMap)(s1,s2) && canMapTerms(varMap, funMap)(t1,t2)
    case (Inequality(s1, t1), Inequality(s2, t2)) =>
      canMapTerms(varMap, funMap)(s1,s2) && canMapTerms(varMap, funMap)(t1,t2)
    case _ => false
  }

  private[this] def areEquivalent(idMap: AuxIriMap,
                                  functionMap: FunctionSymbolMap,
                                  c1: Seq[Literal],
                                  c2: Seq[Literal]): Option[(AuxIriMap, FunctionSymbolMap)] = {

    val (idMapCopy, functionMapCopy) = (idMap.copy, functionMap.copy)

    val varMap = new VariableMap
    val canMap =
      c1.corresponds(c2)(literalsAreEquivalent(idMapCopy, varMap, functionMapCopy))

    if (canMap) Some(idMapCopy, functionMapCopy)
    else None
  }

  private[this] def compare(actual: Seq[STClause], expected: Seq[STClause]): Boolean = {

    // AuxIriMap and FunctionSymbolMap are global for the clause set; VariableMap is on a per-clause basis.
    val initialIdMap = new AuxIriMap
    val initialFunctionMap = new FunctionSymbolMap

    val expectedPermutations: Seq[(Stream[Seq[Predicate]], Stream[Seq[Literal]], Seq[Predicate], Seq[Literal])] =
      (expected zip actual) map {
        case (STClause(body1, head1), STClause(body2, head2)) =>
          (body1.permutations.toStream, head1.permutations.toStream, body2, head2)
      }

    def recurse(idMap: AuxIriMap,
                functionMap: FunctionSymbolMap,
                todo: Seq[(Stream[Seq[Predicate]], Stream[Seq[Literal]], Seq[Predicate], Seq[Literal])]): Boolean =
      todo match {
        case Nil => true
        case (bodyPermutations, headPermutations, body, head) +: rest =>
          bodyPermutations exists {
            bodyPerm =>
              areEquivalent(idMap, functionMap, bodyPerm, body) match {
                case Some((updatedIdMap: AuxIriMap, updatedFunctionMap: FunctionSymbolMap)) =>
                  headPermutations exists {
                    headPerm =>
                      areEquivalent(updatedIdMap, updatedFunctionMap, headPerm, head) match {
                        case Some((updatedIdMap: AuxIriMap, updatedFunctionMap: FunctionSymbolMap)) =>
                          recurse(updatedIdMap, updatedFunctionMap, rest)
                        case None =>
                          false
                      }
                  }
                case None =>
                  false
              }
          }
      }

    actual.size == expected.size && recurse(initialIdMap, initialFunctionMap, expectedPermutations)
  }

  /* TODO
   * Separate the internal IRIs introduced by exists and forall, and only allow some:X to map onto some:Y, and all:X to
   * map onto all:Y.
   */

  private[this] final case class Feature(iriBody: Set[Int], iriHead: Set[Int]) extends Ordered[Feature] {
    override def compare(that: Feature): Int = {
      val thisBodySeq: Seq[Int] = this.iriBody.toSeq.sorted
      val thatBodySeq: Seq[Int] = that.iriBody.toSeq.sorted
      val thisHeadSeq: Seq[Int] = this.iriHead.toSeq.sorted
      val thatHeadSeq: Seq[Int] = that.iriHead.toSeq.sorted
      import scala.math.Ordering.Implicits._
      if (thisBodySeq < thatBodySeq) -1
      else if (thatBodySeq < thisBodySeq) +1
      else if (thisHeadSeq < thatHeadSeq) -1
      else if (thatHeadSeq < thisHeadSeq) +1
      else 0
    }
  }

  class DecoratedSet(val s: Seq[STClause])(implicit assertions: Assertions) {
    override def equals(o: Any): Boolean = o match {
      case that: DecoratedSet if this.s.isEmpty && that.s.isEmpty => true
      case that: DecoratedSet =>

        val thisMap = new mutable.HashMap[Feature, mutable.Set[STClause]] with mutable.MultiMap[Feature, STClause]
        val thatMap = new mutable.HashMap[Feature, mutable.Set[STClause]] with mutable.MultiMap[Feature, STClause]

        val literalToInt: Literal => Int = {
          case p: Predicate =>
            val iri = p.iri
            if (iri.isInternalDisjunct) -1
            else if (iri.isInternalExistential) -2
            else if (iri.isInternalUniversal) -3
            else { val uid = iri.uid; assert(uid >= 0); uid }
          case e: Equality => -4
          case e: Inequality => -5
        }

        def buildMap(clauses: Seq[STClause], map: mutable.MultiMap[Feature, STClause]): Unit = {
          for (c @ STClause(body, head) <- clauses)
            map.addBinding(Feature(body map literalToInt toSet, head map literalToInt toSet), c)
        }

        buildMap(this.s, thisMap)
        buildMap(that.s, thatMap)

        val partialClauseOrder: (STClause, STClause) => Boolean = {
          case (STClause(_, Seq(Inequality(s1, t1))), STClause(_, Seq(Inequality(s2, t2)))) =>
            // Use the global term order.
            s1 < s2 || (s1 == s2 && t1 < t2)
          case (STClause(_, Seq(Inequality(_, _))), _) =>
            true
          case _ =>
            false
        }

        // Pick a permutation for this sequence (but inequality Horn clauses are totally ordered for efficiency).
        val thisSequence: Seq[STClause] = thisMap.keySet.toSeq.sorted.flatMap{thisMap(_).toSeq sortWith partialClauseOrder}

        def relevantPermutations(s: mutable.Set[STClause]): Set[Seq[STClause]] = {
          /* If the all of the clauses in the current bucket are Horn and the head is an inequality (i.e., the form of
           * clauses that result from at-least restrictions), then sort them in an order rather than considering all
           * permutations (which can be prohibitively slow when the cardinality used in the at-least restriction is
           * anything other than very small). */
          val isInequalityClauses = s forall {
            case STClause(_, Seq(Inequality(_, _))) => true
            case _ => false
          }
          if (isInequalityClauses) {
            Set(s.toSeq sortWith partialClauseOrder)
          } else {
            s.toSeq.permutations.toSet
          }
        }

        // Compare against all relevant sequences of the other sequence.
        val thatSequences: Set[Seq[STClause]] = thatMap.keySet.toSeq.sorted.map{f: Feature => relevantPermutations(thatMap(f))}.foldLeft(Set.empty[Seq[STClause]]){
          case (acc: Set[Seq[STClause]], perm: Set[Seq[STClause]]) =>
            if (acc.isEmpty) perm
            else for (sequence: Seq[STClause] <- acc; permutation: Seq[STClause] <- perm) yield sequence ++ permutation
        }

        thatSequences.exists {
          permutation => compare(thisSequence, permutation)
        }

      case _ => throw new IllegalArgumentException("Can only compare decorated sets.")
    }
    override def toString: String =
      // s.map{_.toString}.sorted.mkString(System.lineSeparator, "),"+System.lineSeparator, "),"+System.lineSeparator).replace("http://example.org/","").replace("STClause(","OntologyClause(Body(").replace(" -> ","), Head(").replace(">(",", ").replace(" AND ",", ").replace("<","Concept(").replace("(x)","")
      s.map{_.toString}.sorted.mkString(System.lineSeparator, System.lineSeparator, System.lineSeparator)
  }

  def decorate(s: Set[STClause])(implicit evidence: ClassTag[STClause], assertions: Assertions) = new DecoratedSet(s.toSeq)

  private[this] def toSTClause(c: OntologyClause) = STClause(c.body, c.head)
  private[this] def toSTClause(c: ContextClause) = STClause(c.body, c.head)

  def decorate(s: Set[OntologyClause])(implicit assertions: Assertions) =
    new DecoratedSet(s map toSTClause toSeq)

  def decorate(s: Set[ContextClause])(implicit evidence1: ClassTag[ContextClause], evidence2: ClassTag[ContextClause], assertions: Assertions) =
    new DecoratedSet(s map toSTClause toSeq)

  def transform(axioms: Set[_ <: Axiom]): Set[OntologyClause] = {
    val ontology = new DLOntology(axioms, CommonNames.DoNothingUnsupportedFeatureObserver)
    ontology.getOntologyClauses.toSet
  }

  def transform(axiom: Axiom): Set[OntologyClause] = transform(Set(axiom))

}
