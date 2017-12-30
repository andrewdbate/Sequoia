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

import com.sequoiareasoner.arrayops._
import com.sequoiareasoner.kernel.owl.iri.IRI
import com.sequoiareasoner.kernel.structural.DLOntology

/** Package providing the implementation of predicates, equations, literals, clauses, and ordering.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
package object clauses {

  /** Implementation of the __context literal order__. Furthermore, if the ordering is for a root context,
    * then the implementation also satisfies the order requirements of the __Completeness Theorem__.
    *
    * @param root `true` if this is an order for a root context.
    */
  final class ContextLiteralOrdering private[clauses] (root: Boolean) extends PartialOrdering[Literal] {

    override def tryCompare(x: Literal, y: Literal): Option[Int] =
      // Since we do not call this method, we only provide a short inefficient implementation.
      (lteq(x, y), lteq(y, x)) match {
        case (false, false) => None
        case (false, true)  => Some(+1)
        case (true, false)  => Some(-1)
        case (true, true)   => Some(0)
      }

    override def lteq(o1: Literal, o2: Literal): Boolean = (o1, o2) match {

      case (Equality(l1, r1),   Equality(l2, r2))   => l1 < l2 || (l1 == l2 && r1 <= r2)
      case (Inequality(l1, r1), Inequality(l2, r2)) => l1 < l2 || (l1 == l2 && r1 <= r2)
      case (Equality(l1, r1),   Inequality(l2, r2)) => l1 < l2 || (l1 == l2 && r1 <= r2)
      case (Inequality(l1, r1), Equality(l2, r2))   => l1 < l2 || (l1 == l2 && r1 < r2)

        /* The following ordering between equations and concepts meets the ordering definition requirements because the
         * query clause is only allowed to contain predicates of the form A(x) and the set of pred triggers can only
         * predicates of the form A(y) and R(x, y) and R(y, x) and R(y, y). Therefore only the equations Equality(x, y)
         * and Inequality(x, y) are smaller than any of these aforementioned predicates, and all other equations are
         * greater.
         */
      case (e: Equation, Concept(_, t)) => e.s <= t
      case (Concept(_, t), e: Equation) => !(e.s <= t)
      case (e: Equation, Role(_, s, t)) => e.s <= s || e.s <= t
      case (Role(_, s, t), e: Equation) => !(e.s <= s || e.s <= t)

      case (p1: Predicate, p2: Predicate) if p2.isPredTrigger =>
        /* This is correct because Pred triggers can only be of the form A(y) or R(x, y) or R(y, x), and is thus
         * compatible with the ordering between equations and predicates above. The pred triggers have to all be at the
         * bottom of the partial ordering, according to the ordering definition, and therefore a pred trigger predicate
         * can only be less than or equal to itself.
         */
        p1 == p2

      case (p1: Predicate, p2: Predicate) if p1.isPredTrigger && !p2.isPredTrigger =>
        /* This case exists to try to totalize the order as much as possible. Without it the predicates would be
         * compared using the rules below, and this case could return false otherwise. */
        true

      case (c1: Concept, c2: Concept) if root && c2.isQueryConcept =>
        /* This is correct because query concepts can only be of the form A(x), and is thus compatible with the ordering
         * between equations and predicates above. The query concepts have to all be unordered w.r.t. other query
         * concepts, but can be larger than pred triggers (as per the completeness claim, and guaranteed by the above
         * cases), but can be smaller than other predicates.
         */
        c1 == c2

      case (c1: Concept, c2: Concept) if root && c1.isQueryConcept && !c2.isQueryConcept =>
        /* This case exists to try to totalize the order as much as possible. Without it the query concepts would be
         * compared using the rules below, and this case could return false otherwise. */
        true

      case (p1: Predicate, p2: Predicate) if p1.iri.isInternalDisjunct && !p2.iri.isInternalDisjunct =>
        /* This is an optimisation that is crucial for performance. */
        true

      case (Role(iri1, s1, t1), Role(iri2, s2, t2)) =>
        /* Both of the following are valid orders:
         *   (1) iri < iri2 || (iri == iri2 && (s1 < s2 || (s1 == s2 && t1 < t2)))
         *   (2) s1 < s2 || (s1 == s2 && (t1 < t2 || (t1 == t2 && iri <= iri2)))
         * In practice, reasoning performance will be significantly slower with (1) instead of (2). For an explanation,
         * see the other comment about for concepts below.
         */
        s1 < s2 || (s1 == s2 && (t1 < t2 || (t1 == t2 && iri1 <= iri2)))

      case (Concept(iri1, s), Concept(iri2, t)) =>
        /* Both of the following are valid orders:
         *   (1) iri1 < iri2 || (iri1 == iri2 && s <= t)
         *   (2) s < t || (s == t && iri1 <= iri2)
         * However, while (2) has the property that B(x) < A(f(x)) regardless of the IRI ordering on A and B, (1) does
         * not. In practice, reasoning performance will be significantly slower with (1) instead of (2). We have thus
         * effectively put function symbols higher in the order than predicate symbol when constructing the
         * lexicographic path order, and this is the opposite of the advice given in Section 3.2, Paramodulation-Based
         * Theorem Proving, Handbook of Automated Reasoning, Volume 1 (2001). This ordering reduces the number of
         * applications of Hyper in a context, because only concepts of the form C(x) are matched against clause bodies.
         */
        s < t || (s == t && iri1 <= iri2)

      case (Role(iri1, s1, t1), Concept(iri2, t)) =>
        /* This ordering has also been designed to reduce the number of applications of Hyper. See above. */
        s1 < t || (s1 == t && (t1 < t || (t1 == t && iri1 <= iri2)))

      case (Concept(iri2, t), Role(iri1, s1, t1)) =>
        !(s1 < t || (s1 == t && (t1 < t || (t1 == t && iri1 <= iri2))))

    }

  }

  val RootContextLiteralOrdering = new ContextLiteralOrdering(true)
  val NonRootContextLiteralOrdering = new ContextLiteralOrdering(false)

  sealed trait Literal extends Ordered[Literal] {
    self =>

    type T <: Literal

    /**
      * @return `true` if and only if this literal is a predecessor trigger.
      */
    def isPredTrigger: Boolean

    /**
      * @return `true` if and only if this literal does not contain a function term at any sub-position.
      */
    def isFunctionFree: Boolean

    /** Returns `true` if and only if this literal is an inequality of the form `Inequality(t, t)` for some term `t`.
      * This method can be used to implement to Ineq rule of the calculus.
      *
      * @return `true` if and only if this literal is an inequality of the form `Inequality(t, t)` for some term `t`.
      */
    def isInvalidEquation: Boolean

    /** Returns `true` if and only if this literal is an equality of the form `Equality(t, t)` for some term `t`. This
      * method can be used as part of the implementation of the Elim rule of the calculus.
      *
      * @return `true` if and only if this literal is an equality of the form `Equality(t, t)` for some term `t`
      */
    def isValidEquation: Boolean

    /**
      * @return `true` if and only if the form of this predicate is allowed to occur in the body of a context clause.
      */
    def isLegalInContextClauseBody: Boolean

    /**
      * @return `true` if and only if the form of this predicate is allowed to occur in the head of a context clause.
      */
    def isLegalInContextClauseHead: Boolean

    /**
      * @return `true` if and only if the form of this predicate is allowed to occur in the body of an ontology clause.
      */
    def isLegalInOntologyClauseBody: Boolean

    /**
      * @return `true` if and only if the form of this predicate is allowed to occur in the head of an ontology clause.
      */
    def isLegalInOntologyClauseHead: Boolean

    /** Return `true` if and only if `value` can be represented in `n` bits without loss of information about the
      * magnitude or sign of the value.
      *
      * @param value
      * @param n
      * @return return `true` if and only if `value` can be represented in `n` bits without loss of information.
      */
    protected[this] def canTruncate(value: Int, n: Int): Boolean = {
      require(n > 0)
      // Check whether the (32 - n + 1) highest order bits are all equal.
      val ext = value >> (n - 1)
      ext == 0 || ext == -1
    }

    /** A non-negative long integer that is a unique identifier for this literal.
      *
      * This bit string is such that the three high order bits are:
      * $ - `000` if the literal is a concept;
      * $ - `001` if the literal is a role;
      * $ - `011` if the literal is an equality;
      * $ - `010` if the literal is an inequality.
      * The other bits can be anything, and it is up to the subclasses to determine the layout of the remaining part of
      * the bit string.
      *
      * The high order bit (which is always zero) can be used externally to tag whether this literal occurs positively
      * or negative.
      *
      * @return a non-negative long integer that is a unique identifier for this literal.
      */
    def uid: Long

    /**
      * @return negative if this < that, positive if this > that, zero otherwise (if this == that).
      */
    final override def compare(that: Literal): Int = java.lang.Long.compare(self.uid, that.uid)

    def applySubstitution(sigma: Substitution): T

    def containsAtRewritePosition(l: Term): Boolean

    def rewrite(l: Term, r: Term): T
  }

  /** Returns `true` if each element in the first array is contained in the second array.
    *
    * @param arr1  the array to be checked for containment
    * @param arr2  the array to be tested against
    * @tparam L    the type of the literal
    * @return `true` if each element of `arr1` is contained in `arr2`.
    */
  private[this] def isSubsetOf[L <: Literal](arr1: Array[L], arr2: Array[L]): Boolean = {
    val m = arr1.length
    val n = arr2.length
    if (m > n) return false
    var i = 0; var j = 0
    while (i < m && j < n) {
      val cmp = arr1(i) compare arr2(j)
      if (cmp > 0) {
        j += 1
      } else if (cmp == 0) {
        i += 1
        j += 1
      } else {
        return false
      }
    }
    return i == m
  }

  sealed trait Predicate extends Literal {
    override type T <: Predicate
    def iri: IRI
    def isSuccTrigger: Boolean

    def hasCentralVariable: Boolean

    def maxTerm: Term

    /** Returns a bit string that can be used to quickly determine whether two predicates cannot centrally unify.
      * Given two predicates `p1` and `p2`, and a central substitution `sigma`, the bit string is such that if either
      * `sigma(p1) == p2` or `sigma(p2) == p1` then `p1.centralUnifierPattern == p2.centralUnifierPattern`.
      *
      * @return a bit string that determines whether this predicate can centrally unify with another.
      */
    def centralUnifierPattern: Long

    final override def isInvalidEquation: Boolean = false

    final override def isValidEquation: Boolean = false

  }

  /** Represents a concept  of the form `iri(t)`. */
  final case class Concept(iri: IRI, t: Term)(implicit ontology: DLOntology) extends Predicate {
    override type T = Concept

    // The layout is 000[<-- 31 bits IRI -->][<-- 30 bits t term -->].
    override val uid: Long = {
      assert(canTruncate(iri.uid, 31), "Cannot truncate IRI.")
      assert(canTruncate(t.id, 30), "Cannot truncate term.")
      ((iri.uid.toLong & (-1L >>> 33)) << 30) | (t.id.toLong & (-1L >>> 34))
    }

    override def centralUnifierPattern: Long =
      if (t.isCentralVariable) uid else uid | 0x7FFFFFFFL

    override val hashCode: Int = (uid ^ (uid >>> 32)).toInt

    override def applySubstitution(sigma: Substitution) = Concept(iri, sigma(t))

    override def containsAtRewritePosition(l: Term): Boolean = l == t

    override def rewrite(l: Term, r: Term): Concept =
      if (l == t) Concept(iri, r)
      else throw new IllegalArgumentException

    def copy(t: Term) = new Concept(iri, t)(ontology)

    /**
      * `true` iff A ∈ Su where, for some function symbol f, we have `this` == A{x ↦ f(x), y ↦ x}.
      */
    override def isSuccTrigger: Boolean = t match {
      // The set Su contains B(x) if B(x) occurs in the body of some clause, and never contains C(y) for any C.
      case f if f.isFunctionTerm => ontology.isConceptSuccTrigger(iri)
      case _ => false
    }

    /**
      * `true` iff `this` ∈ Pr.
      */
    override def isPredTrigger: Boolean =
      // The set Pr contains B(y) for each B, and never contains C(x) for any C.
      t.isPredecessorVariable

    override def isFunctionFree: Boolean = !t.isFunctionTerm

    override def hasCentralVariable: Boolean = t.isCentralVariable

    override def maxTerm: Term = t

    override def isLegalInContextClauseBody: Boolean =
      t.isCentralVariable || t.isPredecessorVariable
    override def isLegalInContextClauseHead: Boolean =
      t.isCentralVariable || t.isPredecessorVariable || t.isFunctionTerm
    override def isLegalInOntologyClauseBody: Boolean =
      t.isCentralVariable || (t.isNeighbourVariable && !t.isPredecessorVariable)
    override def isLegalInOntologyClauseHead: Boolean =
      t.isCentralVariable || (t.isNeighbourVariable && !t.isPredecessorVariable) || t.isFunctionTerm

    def isQueryConcept: Boolean = iri.isImported && t.isCentralVariable

    override def toString: String = s"$iri($t)"
  }

  /** Represents a role of the form `iri(s, t)`. */
  final case class Role(iri: IRI, s: Term, t: Term)(implicit ontology: DLOntology) extends Predicate {
    require(s.isCentralVariable || t.isCentralVariable)

    override type T = Role

    /* If s.isCentralVariable then the layout is
     * 0010[<-- 30 bits IRI -->][<-- 30 bits t term -->].
     * Otherwise, if !s.isCentralVariable then the layout is
     * 0011[<-- 30 bits IRI -->][<-- 30 bits s term -->].
     * Note that the UID is unique even in the (currently illegal) case where both s and t are the central variable.
     */
    override val uid: Long = {
      assert(canTruncate(iri.uid, 30), "Cannot truncate IRI.")
      if (s.isCentralVariable) {
        assert(canTruncate(t.id, 30), "Cannot truncate t term.")
        (2L << 60) | ((iri.uid.toLong & (-1L >>> 34)) << 30) | (t.id.toLong & (-1L >>> 34))
      } else {
        assert(canTruncate(s.id, 30), "Cannot truncate s term.")
        (3L << 60) | ((iri.uid.toLong & (-1L >>> 34)) << 30) | (s.id.toLong & (-1L >>> 34))
      }
    }

    def directedRolePattern: Int =
      if (s.isCentralVariable && t.isCentralVariable)
        (1 << 30) | (iri.uid & (-1 >>> 2))
      else if (s.isCentralVariable)
        (2 << 30) | (iri.uid & (-1 >>> 2))
      else
        (3 << 30) | (iri.uid & (-1 >>> 2))

    override def centralUnifierPattern: Long = uid | 0x3FFFFFFFL

    override val hashCode: Int = (uid ^ (uid >>> 32)).toInt

    override def applySubstitution(sigma: Substitution) = Role(iri, sigma(s), sigma(t))

    override def containsAtRewritePosition(l: Term): Boolean = l == s || l == t

    override def rewrite(l: Term, r: Term): Role =
      if (l == s) Role(iri, r, t)
      else if (l == t) Role(iri, s, r)
      else throw new IllegalArgumentException

    def copy(s: Term, t: Term) = new Role(iri, s, t)(ontology)

    /**
      * `true` iff A ∈ Su where, for some function symbol f, we have `this` == A{x ↦ f(x), y ↦ x}.
      */
    override def isSuccTrigger: Boolean =
      /* The set Su contains (as according to the calculus' definition):
       *  - R(x, y) iff R(x, z_i) occurs in the body of some clause for some i, and
       *  - R(y, x) iff R(z_i, x) occurs in the body of some clause for some i.
       */
      (s.isCentralVariable && t.isFunctionTerm && ontology.isBackwardRoleSuccTrigger(iri)) ||
      (t.isCentralVariable && s.isFunctionTerm && ontology.isForwardRoleSuccTrigger(iri))

    /**
      * `true` iff `this` ∈ Pr.
      */
    override def isPredTrigger: Boolean =
      /* The set Pr contains (as according to the calculus' definition):
       *  - R(x, y) iff R(z_i, x) occurs in the body of some clause for some i, and
       *  - R(y, x) iff R(x, z_i) occurs in the body of some clause for some i.
       */
      (s.isCentralVariable && t.isPredecessorVariable && (ontology isBackwardRoleSuccTrigger iri)) ||
      (t.isCentralVariable && s.isPredecessorVariable && (ontology isForwardRoleSuccTrigger iri))
      
    override def isFunctionFree: Boolean = !s.isFunctionTerm && !t.isFunctionTerm

    override def hasCentralVariable: Boolean = true

    override def isLegalInContextClauseBody: Boolean =
      (s.isCentralVariable && t.isPredecessorVariable) ||
        (t.isCentralVariable && s.isPredecessorVariable) ||
        (s.isCentralVariable && t.isCentralVariable)
    override def isLegalInContextClauseHead: Boolean =
      (s.isCentralVariable && (t.isPredecessorVariable || t.isFunctionTerm)) ||
        (t.isCentralVariable && (s.isPredecessorVariable || s.isFunctionTerm)) ||
        (s.isCentralVariable && t.isCentralVariable)
    override def isLegalInOntologyClauseBody: Boolean =
      (s.isCentralVariable && t.isNeighbourVariable && !t.isPredecessorVariable) ||
        (t.isCentralVariable && s.isNeighbourVariable && !s.isPredecessorVariable) ||
        (s.isCentralVariable && t.isCentralVariable)
    override def isLegalInOntologyClauseHead: Boolean =
      (s.isCentralVariable && ((t.isPredecessorVariable && !t.isPredecessorVariable) || t.isFunctionTerm)) ||
        (t.isCentralVariable && ((s.isPredecessorVariable && !s.isPredecessorVariable) || s.isFunctionTerm)) ||
        (s.isCentralVariable && t.isCentralVariable)

    def inverse = Role(iri, t, s)

    override def maxTerm: Term = s max t

    override def toString: String = s"$iri($s, $t)"
  }

  sealed trait Equation extends Literal {
    override type T <: Equation

    /**
      * @return the (non-strict) maximum term of the equation.
      */
    def s: Term

    /**
      * @return the (non-strict) minimum term of the equation.
      */
    def t: Term
    
    assert(s == (s max t) && t == (s min t))

    final override def isLegalInContextClauseBody: Boolean =
      false
    final override def isLegalInContextClauseHead: Boolean =
      s.isFunctionTerm && (t.isPredecessorVariable || t.isFunctionTerm)
    final override def isLegalInOntologyClauseBody: Boolean =
      false
    final override def isLegalInOntologyClauseHead: Boolean =
      s.isFunctionTerm && ((t.isNeighbourVariable && !t.isPredecessorVariable) || t.isFunctionTerm)

    def isPredTrigger: Boolean = false
  }

  /** Represents an equality `s == t` such that either `s > t` or `s == t`.
    *
    * @param s the first term
    * @param t the second term
    */
  final case class Equality(s: Term, t: Term) extends Equation {
    require(s > t || s == t)
    override type T = Equality
    // The layout is 0110[<-- 30 bits s term -->][<-- 30 bits t term -->].
    override val uid: Long = {
      assert(canTruncate(s.id, 30), "Cannot truncate s.")
      assert(canTruncate(t.id, 30), "Cannot truncate t.")
      (6L << 60) | ((s.id.toLong & (-1L >>> 34)) << 30) | (t.id.toLong & (-1L >>> 34))
    }
    override def applySubstitution(sigma: Substitution) = {
      val l = sigma(s); val r = sigma(t)
      if (l > r) Equality(l, r) else Equality(r, l)
    }
    override def containsAtRewritePosition(l: Term): Boolean = l == s
    override def rewrite(l: Term, r: Term): Equality =
      if (l == s && r > t) Equality(r, t)
      else if (l == s) Equality(t, r)
      else throw new IllegalArgumentException
    override def isFunctionFree: Boolean = !s.isFunctionTerm && !t.isFunctionTerm
    override def isInvalidEquation: Boolean = false
    override def isValidEquation: Boolean = s == t
  }

  /** Represents an inequality `s =/= t` such that either `s > t` or `s == t`.
    *
    * @param s the first term
    * @param t the second term
    */
  final case class Inequality(s: Term, t: Term) extends Equation {
    require(s > t || s == t)
    require(s.isFunctionTerm)
    override type T = Inequality
    // The layout is 0100[<-- 30 bits s term -->][<-- 30 bits t term -->].
    override val uid: Long = {
      assert(canTruncate(s.id, 30), "Cannot truncate s.")
      assert(canTruncate(t.id, 30), "Cannot truncate t.")
      (4L << 60) | ((s.id.toLong & (-1L >>> 34)) << 30) | (t.id.toLong & (-1L >>> 34))
    }
    override def applySubstitution(sigma: Substitution) = {
      val l = sigma(s); val r = sigma(t)
      if (l > r) Inequality(l, r) else Inequality(r, l)
    }
    override def containsAtRewritePosition(l: Term): Boolean = l == s
    override def rewrite(l: Term, r: Term): Inequality =
      if (l == s && r > t) Inequality(r, t)
      else if (l == s) Inequality(t, r)
      else throw new IllegalArgumentException
    override def isFunctionFree: Boolean = !s.isFunctionTerm && !t.isFunctionTerm
    override def isInvalidEquation: Boolean = s == t
    override def isValidEquation: Boolean = false
  }

  /** Clause used during structural transformation.
    *
    * @param body
    * @param head
    */
  final case class STClause(body: Seq[Predicate], head: Seq[Literal]) {
    require {
      body forall {
        case Concept(_, v) => v.isVariable
        case Role(_, s, t) => s.isVariable && t.isVariable
      }
    }
    override def toString = s"STClause[${body mkString " AND "} -> ${head mkString " OR "}]"
  }

  object OntologyClause {
    def apply(body: Set[Predicate], head: Set[Literal]): OntologyClause =
      OntologyClause(body.toArray, head.toArray)
    def apply(body: Seq[Predicate], head: Seq[Literal]): OntologyClause =
      // Need to remove duplicates before constructing the ontology clause.
      OntologyClause(body.toSet.toArray, head.toSet.toArray)
    def apply(body: Predicate, head: Literal): OntologyClause =
      OntologyClause(Array(body), Array(head))
  }

  final case class OntologyClause private (body: Array[Predicate], head: Array[Literal]) {
    require(isLegal, s"$this is illegal.")
    override def toString = s"OntologyClause[${body mkString " AND "} -> ${head mkString " OR "}]"

    def isHorn: Boolean = head.length <= 1

    def hasEquality: Boolean =
      cexists(head) {
        case _: Equality => true
        case _ => false
      }

    /**
      * @return `true` iff every neighbour variable occurring in the clause also occurs within a role in the body.
      */
    private[this] def isLegal: Boolean = {
      def termUsageIsLegal(t: Term): Boolean =
        !t.isNeighbourVariable || cexists(body) {
          case Role(_, l, r) => l == t || r == t
          case _ => false
        }
      cforall(body) {
        case Concept(_, v) => v.isCentralVariable
        case _ => true
      } && cforall(head) {
        case Role(_, s, t) => termUsageIsLegal(s) && termUsageIsLegal(t)
        case Concept(_, v) => termUsageIsLegal(v)
        case Equality(s, t) => termUsageIsLegal(s) && termUsageIsLegal(t)
        case Inequality(s, t) => termUsageIsLegal(s) && termUsageIsLegal(t)
      }
    }
  }

  /** Represents a context clause in our calculus (i.e., the clauses that are derived within contexts).
    *
    * @constructor
    * @param body  the predicates in the body of the clause
    * @param head  the literals in the head of the clause
    */
  final case class ContextClause(body: Array[Predicate], head: Array[Literal])(ordering: ContextLiteralOrdering) {

    require {
      // Check that the Ineq rule has always been applied eagerly.
      cforall (head) {
        case Inequality(s, t) if s == t => false
        case _ => true
      }
    }

    def isHorn: Boolean = head.length <= 1
    def isFact: Boolean = body.length == 0 && head.length <= 1

    val maxHeadLiterals: Array[Literal] =
      cfilter (head) { l: Literal => cforall (head) { k => k == l || !ordering.lteq(l, k) } }

    val maxHeadPredicates: Array[Predicate] =
      ccollect[Predicate, Literal] (maxHeadLiterals)

    private[this] def otherHeadLiteral: Array[Literal] = cfilterNot (head ) { l => ccontains (maxHeadLiterals, l) }

    override def toString =
      if (otherHeadLiteral.isEmpty)
        s"ContextClause[${body mkString " AND "} -> [${maxHeadLiterals mkString " OR "}]]"
      else
        s"ContextClause[${body mkString " AND "} -> [${maxHeadLiterals mkString " OR "}] OR ${otherHeadLiteral mkString " OR "}]"

    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

    /** Returns `true` if and only if the head of this clause is a tautology, i.e., there exist terms s and t such that
      * `Equality(s, s) ∈ head` or `{Equality(s, t), Inequality(s, t)} ⊆ head`.
      *
      * @return `true` if and only if the head of this clause is a tautology
      */
    def isHeadTautology: Boolean =
      cexists (head) {
        case Equality(s, t) => s == t || cexists (head) {
          case Inequality(`s`, `t`) => true
          case _ => false
        }
        case _ => false
      }

    /** Determines the strengthening relationship between this clause and the one passed as argument.
      *
      * @param that  the clause to test against
      * @return -1 if `this` is a strengthening of `that` (or `this` and `that` are equal),
      *         +1 if `that` is a proper strengthening of `this`, or
      *          0 is neither is a strengthening of the other.
      */
    def testStrengthening(that: ContextClause): Int = {
      val thisBody = this.body; val thatBody = that.body
      val thisHead = this.head; val thatHead = that.head
      if (isSubsetOf(thisBody, thatBody) && isSubsetOf(thisHead, thatHead)) -1
      else if (isSubsetOf(thatBody, thisBody) && isSubsetOf(thatHead, thisHead)) +1
      else 0
    }

    def isClauseHeadForPred: Boolean = cforall (head) { _.isPredTrigger }

  }

  /** The type of a pred clause with the substitution already applied. */
  final case class PredClause(body: Array[Predicate], head: Array[Predicate]) {
    override def toString = s"PredClause[${body mkString " AND "} -> [${head mkString " OR "}]]"
  }

}
