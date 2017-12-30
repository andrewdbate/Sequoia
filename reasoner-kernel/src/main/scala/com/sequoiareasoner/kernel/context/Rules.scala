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

package com.sequoiareasoner.kernel.context

import com.sequoiareasoner.kernel.index._
import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.arrayops._

import scala.annotation.tailrec

/** Provides an implementation of the Hyper, Pred and Eq rules from the calculus, independent of any particular context.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object Rules {

  @inline private[this] def removeConsecutiveDuplicates[L <: Literal](arr: Array[L], toIndex: Int, @inline builder: Int => Array[L]): Array[L] = {
    var w = -1 // Index of last write.
    crange (0, toIndex) { r =>
      val elem = arr(r)
      if (w == -1 || arr(w).compare(elem) != 0) {
        w += 1
        arr(w) = elem
      }
    }
    val numWrites = w + 1 // Total number of elements written.
    if (numWrites == arr.length) arr else {
      val result = builder(numWrites)
      System.arraycopy(arr, 0, result, 0, numWrites)
      result
    }
  }

  private[this] def sortUnique[L <: Literal](arr: Array[L], toIndex: Int, @inline builder: Int => Array[L]): Array[L] = {
    /* If arr contains nulls at the end then sort will throw NullPointerException. arr will contain nulls at the end due
     * to optimizations to remove invalid equations, cutting, or owl:Nothing. */
    java.util.Arrays.sort(arr.asInstanceOf[Array[AnyRef]], 0, toIndex)
    removeConsecutiveDuplicates(arr, toIndex, builder)
  }

  /**
    * @param p1  predicate from the body of an ontology clause
    * @param p2  predicate from the head of a context clause
    * @return `true` iff there exists a substitution `sigma` such that `p1 \sigma == p2`.
    */
  private[this] def canUnify(p1: Predicate, p2: Predicate): Boolean = (p1, p2) match {
    case (Concept(iri1, t1), Concept(iri2, t2)) =>
      if (!t1.isCentralVariable) throw new IllegalArgumentException
      iri1 == iri2 && t2.isCentralVariable
    case (Role(iri1, s1, t1), Role(iri2, s2, t2)) =>
      iri1 == iri2 && (!s1.isCentralVariable || s2.isCentralVariable) && (!t1.isCentralVariable || t2.isCentralVariable)
    case _ => false
  }

  /** Returns true iff sigma is either such that or can be extended so that `p1 \sigma = p2` holds.
    *
    * @param sigma  central substitution
    * @param p1     predicate over variables only (from body of ontology clause)
    * @param p2     predicate over variables only
    * @return `true` iff `sigma` is such that `p1 \sigma == p2` after the method returns.
    */
  private[this] def unify(sigma: CentralSubstitution, p1: Predicate, p2: Predicate): Boolean = (p1, p2) match {
    case (Concept(iri1, t1), Concept(iri2, t2)) =>
      if (!t1.isCentralVariable) throw new IllegalArgumentException
      iri1 == iri2 && sigma.add(t1, t2)
    case (Role(iri1, s1, t1), Role(iri2, s2, t2)) =>
      /* TODO: an ordering on neighbour variables to avoid repetition in the case of, for example,
       * R(x, z1) AND R(x, z2) -> z1 == z2.
       * We assume that if an ontology clause contains repeated predicates in the body, then the clause is symmetric in
       * the variables occurring in those predicates.
       */
      iri1 == iri2 && sigma.add(s1, s2) && sigma.add(t1, t2)
    case _ => false
  }

  /* Here we use the assumption that each ontology clause is symmetric in neighbour variables (*).
   * To see why this invariant exists, consider the following clauses:
   *
   *                         -> R(x, f(x))  (1)
   *                         -> R(x, g(x))  (2)
   *   R(x, z1) AND R(x, z2) -> C(z2)       (3)
   *                         -> C(f(x))     (4)
   *                         -> C(g(x))     (5)
   *
   * Clause (3) is not symmetric in neighbour variables, and thus violates (*). To derive (4) and (5) from (1), (2) and
   * (3), the head predicate of (1) must be a candidate for unification at both the first and second predicate in the
   * body of (3) even though a match is found at the first predicate. (Similarly for the head predicate of (2).)
   * However, if each clause is symmetric in neighbour variables, then unification tests between a head predicate and
   * the body predicates of another clause can stop after the first match is found.
   */

  /** Implementation of the Hyper rule from the calculus.
    *
    * @param max
    * @param sideConditionToUse
    * @param contextClauseLookup
    * @param ontologyClause
    * @param ordering
    * @param cutting
    * @param isNothing            function that returns `true` if the input is known to be equivalent to `owl:Nothing`.
    * @param resultsBuffer
    */
  def Hyper(max: Predicate,
            sideConditionToUse: ContextClause,
            contextClauseLookup: Predicate => IndexedSequence[ContextClause],
            ontologyClause: OntologyClause,
            ordering: ContextLiteralOrdering,
            cutting: EqualityOptimization,
            isNothing: Literal => Boolean,
            resultsBuffer: UnprocessedDeque): Unit = {
    val ontologyClauseBodyLength = ontologyClause.body.length
    // We have to use the max predicate of sideConditionToUse.
    val sideConditionIndexedSequence = IndexedSequence(sideConditionToUse)
    var haveUsedSideCondition = false
    val arrayContextClauses = new Array[IndexedSequence[ContextClause]](ontologyClauseBodyLength)
    crange(0, ontologyClauseBodyLength) { i =>
      val useSideCondition = !haveUsedSideCondition && canUnify(ontologyClause.body(i), max)
      if (useSideCondition) {
        // The newly derived context clause must be used as a side condition exactly once.
        arrayContextClauses(i) = sideConditionIndexedSequence
        haveUsedSideCondition = true
      } else {
        val candidates: IndexedSequence[ContextClause] = contextClauseLookup(ontologyClause.body(i))
        if (candidates.isEmpty) return
        arrayContextClauses(i) = candidates
      }
    }
    if (!haveUsedSideCondition) throw new AssertionError

    val outerCounter = new Array[Int](ontologyClauseBodyLength)

    @inline @tailrec def incOuterCounter(k: Int = 0): Boolean =
      (k < ontologyClauseBodyLength) && {
        (outerCounter(k) + 1 < arrayContextClauses(k).length && { outerCounter(k) += 1; true }) ||
        { outerCounter(k) = 0; incOuterCounter(k + 1) }
      }

    val innerCounter = new Array[Int](ontologyClauseBodyLength)

    @inline @tailrec def incInnerCounter(k: Int = 0): Boolean =
      (k < ontologyClauseBodyLength) && {
        (innerCounter(k) + 1 < arrayContextClauses(k)(outerCounter(k)).maxHeadPredicates.length && { innerCounter(k) += 1; true }) ||
          { innerCounter(k) = 0; incInnerCounter(k + 1) }
      }

    do {

      do {
        val sigma = new CentralSubstitution
        val isUnifiable =
          cforall (0, ontologyClauseBodyLength) { i: Int =>
            val contextClauseMax: Predicate = arrayContextClauses(i)(outerCounter(i)).maxHeadPredicates(innerCounter(i))
            val bodyCandidate: Predicate = ontologyClause.body(i)
            unify(sigma, bodyCandidate, contextClauseMax)
          }

        if (isUnifiable) {
          val headBufferLength: Int =
            ontologyClause.head.length + csum (0, ontologyClauseBodyLength) { i => arrayContextClauses(i)(outerCounter(i)).head.length - 1 }
          val headBuffer = new Array[Literal](headBufferLength)
          var w = 0
          var isRedundant: Boolean = false
          /* All literals are predicates in the case of the Pred rule so contextual literal cutting cannot apply, and
           * nor can the clause be redundant due to equations in the head. */
          var i = 0
          while (!isRedundant && i < ontologyClause.head.length) {
            val literal: Literal = ontologyClause.head(i)
            val literalSigma: Literal = literal.applySubstitution(sigma)
            if (literalSigma.isValidEquation) {
              isRedundant = true
            } else if (!literalSigma.isInvalidEquation && !cutting.shouldCut(literalSigma) && !isNothing(literalSigma)) {
              headBuffer(w) = literalSigma
              w += 1
            }
            i += 1
          }

          if (!isRedundant) {
            crange (0, ontologyClauseBodyLength) { i =>
              val j = outerCounter(i)
              val k = innerCounter(i)
              val head = arrayContextClauses(i)(j).head
              cforeach (head) { literal =>
                if (arrayContextClauses(i)(j).maxHeadPredicates(k).compare(literal) != 0) {
                  headBuffer(w) = literal
                  w += 1
                }
              }
            }
            val newHead = sortUnique(headBuffer, w, ArrayBuilders.literalArrayBuilder)

            val bodyBufferLength: Int = csum (0, ontologyClauseBodyLength) { i => arrayContextClauses(i)(outerCounter(i)).body.length }
            val bodyBuffer = new Array[Predicate](bodyBufferLength)
            w = 0
            crange (0, ontologyClauseBodyLength) { i =>
              val j = outerCounter(i)
              cforeach (arrayContextClauses(i)(j).body) { predicate =>
                bodyBuffer(w) = predicate
                w += 1
              }
            }
            val newBody = sortUnique(bodyBuffer, w, ArrayBuilders.predicateArrayBuilder)

            val conclusion = ContextClause(newBody, newHead)(ordering)

            if (resultsBuffer.removeRedundant(conclusion)) resultsBuffer.addFirst(conclusion)

          }
        }

      } while (incInnerCounter())

    } while (incOuterCounter())
  }


  /** Implementation of the Pred rule from the calculus.
    *
    * @param contextClauseLookup  function from a predicate `p` to the clauses containing `p` maximally in the head
    * @param predClause           clause pushed back from the successor context to this predecessor context (with substitution applied to bodyu and head)
    * @param ordering             ordering on literals used in this predecessor context
    * @param resultsBuffer        buffer to accumulate results
    */
  def Pred(contextClauseLookup: Predicate => IndexedSequence[ContextClause],
           predClause: PredClause,
           ordering: ContextLiteralOrdering,
           resultsBuffer: UnprocessedDeque): Unit = {
    val predClauseBodyLength = predClause.body.length
    // Because the substitution has been already applied in the pred clause body, no unification is required.
    val arrayContextClauses = new Array[IndexedSequence[ContextClause]](predClauseBodyLength)
    crange(0, predClauseBodyLength) { i =>
      val candidates = contextClauseLookup(predClause.body(i))
      arrayContextClauses(i) = candidates
      if (candidates.isEmpty) return
    }

    val outerCounter = new Array[Int](predClauseBodyLength)
    @inline @tailrec def incOuterCounter(k: Int = 0): Boolean =
      (k < predClauseBodyLength) && {
        (outerCounter(k) + 1 < arrayContextClauses(k).length && { outerCounter(k) += 1; true }) ||
          { outerCounter(k) = 0; incOuterCounter(k + 1) }
      }

    do {
      // Set the buffer to the maximum length of the head of the new clause.
      val headBufferLength: Int =
        predClause.head.length + csum (0, predClauseBodyLength) { i => arrayContextClauses(i)(outerCounter(i)).head.length - 1 }
      val headBuffer = new Array[Literal](headBufferLength)
      // Head of a pred clause contains only predicates, so neither the Ineq rule nor contextual literal cutting can apply.
      var w = 0
      cforeach (predClause.head) { literal =>
        headBuffer(w) = literal
        w += 1
      }
      // Copy heads of clauses excluding matched predicates.
      crange (0, predClauseBodyLength) { i =>
        val j = outerCounter(i)
        val predClauseBodyPredicate = predClause.body(i)
        val head = arrayContextClauses(i)(j).head
        cforeach (head) { literal =>
          if (literal.compare(predClauseBodyPredicate) != 0) {
            headBuffer(w) = literal
            w += 1
          }
        }
      }
      val newHead = sortUnique(headBuffer, w, ArrayBuilders.literalArrayBuilder)

      val bodyBufferLength: Int = csum (0, predClauseBodyLength) { i => arrayContextClauses(i)(outerCounter(i)).body.length }
      val bodyBuffer = new Array[Predicate](bodyBufferLength)
      w = 0
      crange (0, predClauseBodyLength) { i =>
        val j = outerCounter(i)
        cforeach (arrayContextClauses(i)(j).body) { predicate =>
          bodyBuffer(w) = predicate
          w += 1
        }
      }
      val newBody = sortUnique(bodyBuffer, w, ArrayBuilders.predicateArrayBuilder)

      val conclusion = ContextClause(newBody, newHead)(ordering)

      if (resultsBuffer.removeRedundant(conclusion)) resultsBuffer.addFirst(conclusion)

    } while (incOuterCounter())
  }

  def Eq(max: Literal,
         contextClause: ContextClause,
         equality: Equality,
         equalityContextClause: ContextClause,
         ordering: ContextLiteralOrdering,
         cutting: EqualityOptimization,
         resultsBuffer: UnprocessedDeque): Unit = {
    if (!max.containsAtRewritePosition(equality.s)) throw new IllegalArgumentException

    val headBufferLength: Int = contextClause.head.length + equalityContextClause.head.length - 1
    val headBuffer = new Array[Literal](headBufferLength)
    var w = 0

    max match {
      case _: Predicate =>
        headBuffer(w) = max.rewrite(equality.s, equality.t)
        w += 1
      case Equality(equality.s, t2) if equality.t == t2 =>
        return // The new clause is redundant.
      case Equality(equality.s, t2) =>
        if (!cutting.shouldCutEquality(equality.t, t2)) {
          headBuffer(w) = max.rewrite(equality.s, equality.t)
          w += 1
        }
      case Inequality(equality.s, t2) =>
        // Apply Ineq rule immediately to the result.
        if (equality.t != t2 && !cutting.shouldCutInequality(equality.t, t2)) {
          headBuffer(w) = max.rewrite(equality.s, equality.t)
          w += 1
        }
      case _ => throw new IllegalArgumentException("Eq rule is not applicable.")
    }

    cforeach (contextClause.head) { literal =>
      if (literal.compare(max) != 0) {
        headBuffer(w) = literal
        w += 1
      }
    }
    cforeach (equalityContextClause.head) { literal =>
      if (literal.compare(equality) != 0) {
        headBuffer(w) = literal
        w += 1
      }
    }
    val newHead = sortUnique(headBuffer, w, ArrayBuilders.literalArrayBuilder)

    val bodyBufferLength: Int = contextClause.body.length + equalityContextClause.body.length
    val bodyBuffer = new Array[Predicate](bodyBufferLength)
    System.arraycopy(contextClause.body, 0, bodyBuffer, 0, contextClause.body.length)
    System.arraycopy(equalityContextClause.body, 0, bodyBuffer, contextClause.body.length, equalityContextClause.body.length)
    val newBody = sortUnique(bodyBuffer, bodyBufferLength, ArrayBuilders.predicateArrayBuilder)

    val conclusion = ContextClause(newBody, newHead)(ordering)

    if (!conclusion.isHeadTautology) resultsBuffer.addFirst(conclusion)
  }

}
