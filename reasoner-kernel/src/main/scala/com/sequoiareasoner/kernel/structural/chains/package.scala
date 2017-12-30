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

package com.sequoiareasoner.kernel.structural

import com.sequoiareasoner.kernel.owl.model._
import com.sequoiareasoner.kernel.reasoner.SequoiaRuntimeException

/**
  * @author Andrew Bate <code@andrewbate.com>
  */
package object chains {

  final case class NonRegularPropertyHierarchyException(op: ObjectProperty)
    extends SequoiaRuntimeException(s"The given property hierarchy is not regular. There is a cyclic dependency involving $op.")

  /** The supertype of exceptions to be thrown if `op` is not a simple property but is used in a position where
    * only simple properties are allowed.
    *
    * The parent axiom is included together with the axioms that lead to `op` being non-simple in order to provide a
    * useful error to the user.
    *
    * @param op
    * @param axiom             the axiom from which `op` or its inverse was found.
    * @param compositeAxioms
    */
  sealed abstract class UseOfNonSimplePropertyException(op: ObjectProperty, axiom: Axiom, compositeAxioms: Iterable[Axiom])
    extends SequoiaRuntimeException(s"Axiom $axiom uses property $op that is non-simple due to ${compositeAxioms.mkString("[",", ","]")}.")

  /** Exception to be thrown when a property that is not simple is used in an asymmetric object property axiom.
    *
    * @param axiom             the asymmetric object property axiom from which `op` or its inverse was found.
    * @param compositeAxioms
    */
  final case class UseOfNonSimplePropertyInAsymmetricObjectPropertyAxiom(axiom: AsymmetricObjectPropertyAxiom, compositeAxioms: Iterable[Axiom])
    extends UseOfNonSimplePropertyException(axiom.ope.namedProperty, axiom, compositeAxioms)

  /** Exception to be thrown when a property that is not simple is used in a disjoint object properties axiom.
    *
    * @param op                an object property that is not simple and occurs in the supplied disjoint object properties axiom.
    * @param axiom             the disjoint object properties axiom from which `op` or its inverse was found.
    * @param compositeAxioms
    */
  final case class UseOfNonSimplePropertyInDisjointObjectPropertiesAxiom(op: ObjectProperty, axiom: DisjointObjectPropertiesAxiom, compositeAxioms: Iterable[Axiom])
    extends UseOfNonSimplePropertyException(op, axiom, compositeAxioms)

  /** Exception to be thrown when a property that is not simple is used in a functional object property axiom.
    *
    * @param axiom             the functional object property axiom from which `op` or its inverse was found.
    * @param compositeAxioms
    */
  final case class UseOfNonSimplePropertyInFunctionalObjectPropertyAxiom(axiom: FunctionalObjectPropertyAxiom, compositeAxioms: Iterable[Axiom])
    extends UseOfNonSimplePropertyException(axiom.ope.namedProperty, axiom, compositeAxioms)

  /** Exception to be thrown when a property that is not simple is used in an inverse functional object property axiom.
    *
    * @param axiom             the inverse functional object property axiom from which `op` or its inverse was found.
    * @param compositeAxioms
    */
  final case class UseOfNonSimplePropertyInInverseFunctionalObjectPropertyAxiom(axiom: InverseFunctionalObjectPropertyAxiom, compositeAxioms: Iterable[Axiom])
    extends UseOfNonSimplePropertyException(axiom.ope.namedProperty, axiom, compositeAxioms)

  /** Exception to be thrown when a property that is not simple is used in an irreflexive object property axiom.
    *
    * @param axiom             the irreflexive object property axiom from which `op` or its inverse was found.
    * @param compositeAxioms
    */
  final case class UseOfNonSimplePropertyInIrreflexiveObjectPropertyAxiom(axiom: IrreflexiveObjectPropertyAxiom, compositeAxioms: Iterable[Axiom])
    extends UseOfNonSimplePropertyException(axiom.ope.namedProperty, axiom, compositeAxioms)


  final case class UseOfNonSimplePropertyInCardinalityRestriction(op: ObjectProperty, axiom: Axiom, compositeAxioms: Iterable[Axiom])
    extends UseOfNonSimplePropertyException(op, axiom, compositeAxioms)

  final case class UseOfNonSimplePropertyInObjectHasSelf(op: ObjectProperty, axiom: Axiom, compositeAxioms: Iterable[Axiom])
    extends UseOfNonSimplePropertyException(op, axiom, compositeAxioms)

}
