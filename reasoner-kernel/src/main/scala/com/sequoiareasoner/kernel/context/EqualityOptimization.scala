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

import com.sequoiareasoner.kernel.index.TermUnionFind
import com.sequoiareasoner.kernel.clauses._

import scala.collection.mutable

/** The type of all optimizations for equality reasoning.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
trait EqualityOptimization {

  /**
    * @param l  a term
    * @param r  a term
    * @return `true` if and only if contextual literal cutting allows for `Equality(l, r)` or `Equality(r, l)` to be deleted from the head of any clause
    */
  def shouldCutEquality(l: Term, r: Term): Boolean

  def shouldCutInequality(l: Term, r: Term): Boolean

  /**
    * PRECONDITION: `literal` is not an equality of the form `Equality(t, t)` for any term `t`.
    *
    * The precondition exists because if `Equality(t, t)` occurs in the head for any term `t`, then the entire clause is
    * redundant and the clause should instead by deleted, not cut.
    *
    * @param literal  a literal
    * @return `true` if and only if context literal cutting allows `l` to be deleted from the head of any clause
    */
  def shouldCut(literal: Literal): Boolean

  def clauseDerived(clause: ContextClause): Unit

}

object EqualityOptimizationDisabled extends EqualityOptimization {
  override def shouldCutEquality(l: Term, r: Term): Boolean = false
  override def shouldCutInequality(l: Term, r: Term): Boolean = false
  override def shouldCut(literal: Literal): Boolean = false
  override def clauseDerived(clause: ContextClause): Unit = {}
}


final class SimplifyReflectEqualityOptimization extends EqualityOptimization {

  /**
    * `negativeSimplifyReflect(l) contains r` if `Equality(l, r)` can be deleted from the head of any clause.
    */
  private[this] val negativeSimplifyReflect: mutable.MultiMap[Term, Term] = // TODO: replace with a fast IntIntMultiMap
    new mutable.HashMap[Term, mutable.Set[Term]] with mutable.MultiMap[Term, Term] {
      override def default(l: Term) = mutable.Set.empty[Term]
    }

  private[this] val positiveSimplifyReflect: TermUnionFind = new TermUnionFind

  override def shouldCutEquality(l: Term, r: Term): Boolean = {
    require {
      l != r &&
        (l.isCentralVariable || l.isPredecessorVariable || l.isFunctionTerm) &&
        (r.isCentralVariable || r.isPredecessorVariable || r.isFunctionTerm)
    }
    if (r <= l) negativeSimplifyReflect(l) contains r
    else negativeSimplifyReflect(r) contains l
  }

  override def shouldCutInequality(l: Term, r: Term): Boolean = {
    require {
      l != r &&
        (l.isCentralVariable || l.isPredecessorVariable || l.isFunctionTerm) &&
        (r.isCentralVariable || r.isPredecessorVariable || r.isFunctionTerm)
    }
    positiveSimplifyReflect.isConnected(l, r)
  }

  override def shouldCut(literal: Literal): Boolean = literal match {
    case Equality(l, r) => shouldCutEquality(l, r)
    case Inequality(l, r) => shouldCutInequality(l, r)
    case _ => false
  }

  override def clauseDerived(clause: ContextClause): Unit =
    if (clause.body.length == 0 && clause.head.length == 1) clause.head(0) match { // TODO: use isFact()
      case Inequality(l, r) if l != r => negativeSimplifyReflect.addBinding(l, r)
      case Equality(l, r) if l != r => positiveSimplifyReflect.union(l, r)
      case _ => // Do nothing
    }

}
