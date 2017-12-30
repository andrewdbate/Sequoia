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

package com.sequoiareasoner.kernel.owl.iri

import com.sequoiareasoner.kernel.owl.model.{AnnotationSubject, AnnotationValue}

import scala.collection.mutable

object IRI {
  /* An IRI is either an imported IRI (i.e. it originates from an input ontology), or it is an internal IRI (i.e. an
   * IRI that was introduced during structural transformation). An internal IRI is introduced for each quantifier
   * (either universal or existential), and for each class expression in a disjunction. (It is assumed that class
   * expressions are in negation normal form.) Each IRI is uniquely identified by an integer. In order to quickly
   * determine the type of an IRI, the following convention on the bit pattern of UIDs is used:
   *  (0|1)^30 00  can be used to represent an imported IRI;
   *  (0|1)^30 01  can be used to represent an internal IRI introduced by an existential quantifier;
   *  (0|1)^30 11  can be used to represent an internal IRI introduced by a universal quantifier;
   *  (0|1)^30 10  can be used to represent an internal IRI introduced by a disjunct.
   * Furthermore, the first few imported IRI UIDs are reserved for IRIs that are predefined in the OWL 2 specification.
   */

  private[this] val registry = new mutable.HashMap[String, Int]
  private[this] val iriLookup = new mutable.LongMap[String]

  private[this] var nextImportedIriUid: Int = 0
  private[this] var nextInternalExistentialUid: Int = 0
  private[this] var nextInternalDisjunctionUid: Int = 0
  private[this] var nextInternalUniversalUid: Int = 0

  /** Creates an IRI corresponding to the full IRI given as a String. This method is not thread safe.
    *
    * @param fullIri  the String that specifies the IRI
    * @return the IRI corresponding to the full IRI given as a String.
    */
  def apply(fullIri: String): IRI = { // TODO: add assertion that IRI is legal (e.g. "<http://www.example.com>" with < and > is illegal.
    val uid = registry.getOrElseUpdate(fullIri, {
      val uid = nextImportedIriUid << 2 // Imported IRI UIDs end in 00.
      nextImportedIriUid += 1
      iriLookup.put(uid, fullIri)
      uid
    })
    require(uid >= 0)
    new IRI(uid)
  }

  /** Creates an IRI by concatenating a prefix with a suffix. This method is not thread safe.
    *
    * @param prefix
    * @param localName
    * @return the IRI whose full String consists of the specified prefix followed by the localName.
    */
  def apply(prefix: Prefix, localName: String): IRI = apply(prefix.iri.fullIriAsString + localName)

  /** Returns an IRI to be used by an auxiliary predicate representing an existential quantifier filler.
    * This method is not thread safe.
    *
    * @return an IRI to be used by an auxiliary predicate representing an existential quantifier filler.
    */
  def some(): IRI = {
    val uid = (nextInternalExistentialUid << 2) + 1 // Internal existential IRI UIDs end in 01.
    nextInternalExistentialUid += 1
    require(uid >= 0)
    new IRI(uid)
  }

  /** Returns an IRI to be used by an auxiliary predicate representing an disjunct.
    * This method is not thread safe.
    *
    * @return an IRI to be used by an auxiliary predicate representing an disjunct.
    */
  def disjunct(): IRI = {
    val uid = (nextInternalDisjunctionUid << 2) + 2 // Internal disjunction IRI UIDs end in 10.
    nextInternalDisjunctionUid += 1
    require(uid >= 0)
    new IRI(uid)
  }

  /** Returns an IRI to be used by an auxiliary predicate representing a universal quantifier filler.
    * This method is not thread safe.
    *
    * @return an IRI to be used by an auxiliary predicate representing a universal quantifier filler.
    */
  def all(): IRI = {
    val uid = (nextInternalUniversalUid << 2) + 3 // Internal universal IRI UIDs end in 11.
    nextInternalUniversalUid += 1
    require(uid >= 0)
    new IRI(uid)
  }

  /** The predefined IRI `http://www.w3.org/2002/07/owl#Thing`. */
  final val owlThing                = apply("http://www.w3.org/2002/07/owl#Thing")
  /** The predefined IRI `http://www.w3.org/2002/07/owl#Nothing`. */
  final val owlNothing              = apply("http://www.w3.org/2002/07/owl#Nothing")
  /** The predefined IRI `http://www.w3.org/2002/07/owl#topObjectProperty`. */
  final val owlTopObjectProperty    = apply("http://www.w3.org/2002/07/owl#topObjectProperty")
  /** The predefined IRI `http://www.w3.org/2002/07/owl#bottomObjectProperty`. */
  final val owlBottomObjectProperty = apply("http://www.w3.org/2002/07/owl#bottomObjectProperty")
  /** The predefined IRI `http://www.w3.org/2002/07/owl#topDataProperty`. */
  final val owlTopDataProperty      = apply("http://www.w3.org/2002/07/owl#topDataProperty")
  /** The predefined IRI `http://www.w3.org/2002/07/owl#bottomDataProperty`. */
  final val owlBottomDataProperty   = apply("http://www.w3.org/2002/07/owl#bottomDataProperty")
  /** The predefined IRI `http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral`. */
  final val rdfPlainLiteral         = apply("http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral")
  /** The predefined IRI `http://www.w3.org/2000/01/rdf-schema#Literal`. */
  final val rdfsLiteral             = apply("http://www.w3.org/2000/01/rdf-schema#Literal")

  /** Given the UID of an imported IRI (including IRIs predefined in the OWL 2 specification), returns the full IRI as a
    * string.
    *
    * @param uid  the UID of the imported IRI
    * @return  the full IRI as a string
    */
  private def fullImportedIriAsString(uid: Int): String = iriLookup(uid)
}

/** Represents International Resource Identifiers.
  *
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param uid a unique identifier generated from `fullIriAsString`
  */
class IRI /*private*/ (val uid: Int) extends AnyVal with AnnotationSubject with AnnotationValue { // FIXME: add an apply local to kernel for this constructor

  // These tests implement the convention on UIDs described above.
  @inline def isImported: Boolean = (uid & 3) == 0
  @inline def isInternal: Boolean = (uid & 3) != 0
  @inline def isInternalExistential: Boolean = (uid & 3) == 1
  @inline def isInternalUniversal: Boolean = (uid & 3) == 3
  @inline def isInternalDisjunct: Boolean = (uid & 3) == 2

  /**
    * @return the full IRI as a string
    */
  def fullIriAsString: String =
    if (isImported) IRI.fullImportedIriAsString(uid)
    else if (isInternalExistential) "some:" + (uid >> 2)
    else if (isInternalUniversal) "all:" + (uid >> 2)
    else "disjunct:" + (uid >> 2)

  /** Returns `true` if this IRI is equal to `owl:Thing`.
    *
    * @return `true` if and only if this IRI is equal to `http://www.w3.org/2002/07/owl#Thing`.
    */
  @inline def isThing: Boolean =
    this == IRI.owlThing

  /** Returns `true` if this IRI is equal to `owl:Nothing`.
    *
    * @return `true` if and only if this IRI is equal to `http://www.w3.org/2002/07/owl#Nothing`.
    */
  @inline def isNothing: Boolean =
    this == IRI.owlNothing

  /** Returns `true` if this IRI is equal to `rdf:PlainLiteral`.
    *
    * @return `true` if and only if this IRI is equal to `http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral`.
    */
  @inline def isPlainLiteral: Boolean =
    this == IRI.rdfPlainLiteral

  /** Returns `true` if this IRI is equal to `rdfs:Literal`.
    *
    * @return `true` if and only if this IRI is equal to `http://www.w3.org/2000/01/rdf-schema#Literal`.
    */
  @inline def isRDFSLiteral: Boolean =
    this == IRI.rdfsLiteral

  /** Compares this IRI with the specified IRI for order.
    *
    * The order implemented is the following: `iri1 <= iri2` if and only if one of the following conditions hold:
    * $ - `iri1` is an internal IRI and `iri2` is an imported IRI;
    * $ - `iri1` is an internal disjunct IRI and `iri2` either an internal existential IRI or an internal universal IRI;
    * $ - `iri1` is an internal existential IRI and `iri2` is an internal universal IRI;
    * $ - `iri1` and `iri2` are both imported IRIs and `iri1.uid <= iri2.uid`;
    * $ - `iri1` and `iri2` are both internal universal IRIs and `iri1.uid <= iri2.uid`;
    * $ - `iri1` and `iri2` are both internal existential IRIs and `iri1.uid <= iri2.uid`;
    * $ - `iri1` and `iri2` are both internal disjunct IRIs and `iri1.uid <= iri2.uid`.
    *
    * Hence internal disjunct IRIs are smaller in the order than other types of IRI. This helps to avoid performing
    * yper-resolution (either in the Hyper rule or the Pred rule) with more than one of the internal predicates that
    * occurs in the head of a disjunctive clause.
    *
    * For example, suppose you have a set of ontology clauses which includes the following
    * {{{
    *   A(x) -> Disjunct_1(x) OR Disjunct_2(x) OR ... OR Disjunct_n(x)
    *   Disjunct_1(x) -> Some_1(x, f_1(x))
    *   ...
    *   Disjunct_n(x) -> Some_n(x, f_n(x))
    *   Some_1(z, x) -> R(z, x)
    *   ...
    *   Some_n(z, x) -> R(z, x)
    *   Some_1(z, x) -> B_1(x)
    *   ...
    *   Some_n(z, x) -> B_n(x)
    *   R(z, x) AND B_1(x) -> C_1(z)
    *   ...
    *   R(z, x) AND B_n(x) -> C_n(z)
    *   R(z, x) AND B_1(x) -> All_1(z)
    *   ...
    *   R(z, x) AND B_n(x) -> All_n(z)
    * }}}
    * where `Some_1` is an internal existential IRI, `All_1` is an internal universal IRI, and `Disjunct_1` through
    * `Disjunct_n` are internal disjunct IRIs.
    *
    * In a context whose core contains `A(x)`, the Core and Hyper rules will derive the following:
    * {{{
    *   \top -> A(x)
    *   \top -> Disjunct_1(x) OR Disjunct_2(x) OR ... OR Disjunct_n(x)
    * }}}
    * Suppose that `Disjunct_1(x)` is maximal in the head. Then the Hyper rule will derive:
    * {{{
    *   \top -> Some_1(x, f_1(x)) OR Disjunct_2(x) OR ... OR Disjunct_n(x)
    * }}}
    * Since the order on literals has to be compatible with the lexicographic path ordering, `Some_1(x, f_1(x))` will be
    * maximal in the head. The Succ rule will introduce an successor context; suppose that the core contains
    * `Some_1(y, x)`. In the successor context you will derive the following using the Core and Hyper rules:
    * {{{
    *   \top -> Some_1(y, x)
    *   \top -> R_1(y, x)
    *   \top -> B_1(x)
    *   \top -> C_1(y)
    *   \top -> All_1(y)
    * }}}
    * Suppose that, due to other axioms in the input ontology, that both `C_1(y)` and `All_1(y)` are predecessor
    * triggers. Then, in the predecessor context (whose core contains `A(x)`) the Pred rule will derive the following:
    * {{{
    *   \top -> C_1(x) OR Disjunct_2(x) OR ... OR Disjunct_n(x)
    *   \top -> All_1(x) OR Disjunct_2(x) OR ... OR Disjunct_n(x)
    * }}}
    * Now the IRI ordering will determine which literal(s) in the head of these last two clauses are maximal.
    * If both imported IRIs and internal universal IRIs are larger in the order than internal disjunct IRIs (as we have
    * implemented it here) then `C_1(x)` and `All_1(x)` will be maximal. Otherwise, if internal disjunct IRIs are allowed
    * to be larger in the order, then, say, `Disjunct_2(x)` will be maximal and the Pred rule will derive the following:
    * {{{
    *   \top -> C_1(x) OR C_2(x) OR ... OR Disjunct_n(x)
    *   \top -> C_1(x) OR All_2(x) OR ... OR Disjunct_n(x)
    *   \top -> All_1(x) OR C_2(x) OR ... OR Disjunct_n(x)
    *   \top -> All_1(x) OR All_2(x) OR ... OR Disjunct_n(x)
    * }}}
    * Repeating this for each of the remaining `Disjunct_i(x)` will result in exponential blowup, which could have been
    * avoided by choosing the ordering described here. Hence, internal disjunct IRIs are always smallest in the ordering.
    *
    * @param that the IRI to be compared.
    * @return `true` if and only if `this` is less than or equal to `that`.
    */
  def <=(that: IRI): Boolean =
    if (that.isImported) {
      this.isInternal || this.uid - that.uid <= 0
    } else if (that.isInternalUniversal) {
      this.isInternal && (!this.isInternalUniversal || this.uid - that.uid <= 0)
    } else if (that.isInternalExistential) {
      this.isInternalDisjunct || (this.isInternalExistential && this.uid - that.uid <= 0)
    } else  {
      assert(that.isInternalDisjunct)
      this.isInternalDisjunct && this.uid - that.uid <= 0
    }

  override def toString: String = "<" + fullIriAsString + ">"

}
