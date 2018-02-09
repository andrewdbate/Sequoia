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

import com.sequoiareasoner.kernel.clauses._
import com.sequoiareasoner.kernel.logging.Logger
import com.sequoiareasoner.kernel.owl.iri.{IRI, Prefix}
import com.sequoiareasoner.kernel.owl.model.{OWLClass, ObjectProperty}
import com.sequoiareasoner.kernel.structural.UnsupportedFeatureObserverIgnore

import scala.language.implicitConversions

/** Object containing IRI prefixes, object properties, classes and terms that are commonly used in the testing
  * of the kernel module.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
object CommonNames {
  import Term._

  val p = Prefix(":", IRI("http://example.org/"))
  val owl = Prefix("owl:", IRI("http://www.w3.org/2002/07/owl#"))
  val rdf = Prefix("rdf:", IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
  val xml = Prefix("xml:", IRI("http://www.w3.org/XML/1998/namespace"))
  val xsd = Prefix("xsd:", IRI("http://www.w3.org/2001/XMLSchema#"))
  val rdfs = Prefix("rdfs:", IRI("http://www.w3.org/2000/01/rdf-schema#"))

  val A = OWLClass(IRI(p, "A"))
  val B = OWLClass(IRI(p, "B"))
  val C = OWLClass(IRI(p, "C"))
  val D = OWLClass(IRI(p, "D"))
  val P = ObjectProperty(IRI(p, "P"))
  val invP = ObjectProperty(IRI(p, "invP"))
  val P1 = ObjectProperty(IRI(p, "P1"))
  val P2 = ObjectProperty(IRI(p, "P2"))
  val P3 = ObjectProperty(IRI(p, "P3"))
  val Q = ObjectProperty(IRI(p, "Q"))
  val invQ = ObjectProperty(IRI(p, "invQ"))
  val R = ObjectProperty(IRI(p, "R"))
  val invR = ObjectProperty(IRI(p, "invR"))
  val S = ObjectProperty(IRI(p, "S"))
  val invS = ObjectProperty(IRI(p, "invS"))
  val S1 = ObjectProperty(IRI(p, "S1"))
  val S2 = ObjectProperty(IRI(p, "S2"))
  val S3 = ObjectProperty(IRI(p, "S3"))
  val Nothing = OWLClass(IRI("http://www.w3.org/2002/07/owl#Nothing"))
  val Thing = OWLClass(IRI("http://www.w3.org/2002/07/owl#Thing"))
  val topObjectProperty = ObjectProperty(IRI("http://www.w3.org/2002/07/owl#topObjectProperty"))

  val R0 = ObjectProperty(IRI(p, "R0"))
  val R1 = ObjectProperty(IRI(p, "R1"))
  val R2 = ObjectProperty(IRI(p, "R2"))
  val R3 = ObjectProperty(IRI(p, "R3"))
  val R4 = ObjectProperty(IRI(p, "R4"))
  val R5 = ObjectProperty(IRI(p, "R5"))
  val R6 = ObjectProperty(IRI(p, "R6"))
  val R7 = ObjectProperty(IRI(p, "R7"))
  val R8 = ObjectProperty(IRI(p, "R8"))
  val R9 = ObjectProperty(IRI(p, "R9"))
  val T = ObjectProperty(IRI(p, "T"))
  val H = ObjectProperty(IRI(p, "H"))
  val H1 = ObjectProperty(IRI(p, "H1"))
  val U = ObjectProperty(IRI(p, "U"))
  val V1 = ObjectProperty(IRI(p, "V1"))
  val V2 = ObjectProperty(IRI(p, "V2"))
  val V3 = ObjectProperty(IRI(p, "V3"))
  val E = OWLClass(IRI(p, "E"))
  val F = OWLClass(IRI(p, "F"))
  val G = OWLClass(IRI(p, "G"))
  val K = OWLClass(IRI(p, "K"))
  val K1 = OWLClass(IRI(p, "K1"))
  val K2 = OWLClass(IRI(p, "K2"))
  val L = OWLClass(IRI(p, "L"))
  val X = OWLClass(IRI(p, "X"))
  val X0 = OWLClass(IRI(p, "X0"))
  val X1 = OWLClass(IRI(p, "X1"))
  val X2 = OWLClass(IRI(p, "X2"))
  val X3 = OWLClass(IRI(p, "X3"))
  val X4 = OWLClass(IRI(p, "X4"))
  val X5 = OWLClass(IRI(p, "X5"))
  val X6 = OWLClass(IRI(p, "X6"))
  val X7 = OWLClass(IRI(p, "X7"))
  val X8 = OWLClass(IRI(p, "X8"))
  val X9 = OWLClass(IRI(p, "X9"))
  val Y = OWLClass(IRI(p, "Y"))
  val Y1 = OWLClass(IRI(p, "Y1"))
  val Y2 = OWLClass(IRI(p, "Y2"))
  val Y3 = OWLClass(IRI(p, "Y3"))
  val Y4 = OWLClass(IRI(p, "Y4"))
  val Y5 = OWLClass(IRI(p, "Y5"))
  val Y6 = OWLClass(IRI(p, "Y6"))
  val Y7 = OWLClass(IRI(p, "Y7"))
  val Y8 = OWLClass(IRI(p, "Y8"))
  val Y9 = OWLClass(IRI(p, "Y9"))
  val Z = OWLClass(IRI(p, "Z"))


  val A0 = OWLClass(IRI(p, "A0"))
  val A1 = OWLClass(IRI(p, "A1"))
  val A2 = OWLClass(IRI(p, "A2"))
  val A3 = OWLClass(IRI(p, "A3"))
  val A4 = OWLClass(IRI(p, "A4"))
  val A5 = OWLClass(IRI(p, "A5"))
  val A6 = OWLClass(IRI(p, "A6"))
  val A7 = OWLClass(IRI(p, "A7"))
  val A8 = OWLClass(IRI(p, "A8"))
  val A9 = OWLClass(IRI(p, "A9"))
  val A10 = OWLClass(IRI(p, "A10"))
  val A11 = OWLClass(IRI(p, "A11"))
  val A12 = OWLClass(IRI(p, "A12"))
  val A13 = OWLClass(IRI(p, "A13"))
  val A14 = OWLClass(IRI(p, "A14"))
  val A15 = OWLClass(IRI(p, "A15"))
  val A16 = OWLClass(IRI(p, "A16"))
  val A17 = OWLClass(IRI(p, "A17"))
  val A18 = OWLClass(IRI(p, "A18"))
  val A19 = OWLClass(IRI(p, "A19"))
  val A20 = OWLClass(IRI(p, "A20"))
  val A21 = OWLClass(IRI(p, "A21"))
  val A22 = OWLClass(IRI(p, "A22"))
  val A23 = OWLClass(IRI(p, "A23"))
  val A24 = OWLClass(IRI(p, "A24"))
  val A25 = OWLClass(IRI(p, "A25"))
  val A26 = OWLClass(IRI(p, "A26"))
  val A27 = OWLClass(IRI(p, "A27"))
  val A28 = OWLClass(IRI(p, "A28"))
  val A29 = OWLClass(IRI(p, "A29"))
  val A30 = OWLClass(IRI(p, "A30"))
  val A31 = OWLClass(IRI(p, "A31"))

  val B0 = OWLClass(IRI(p, "B0"))
  val B1 = OWLClass(IRI(p, "B1"))
  val B2 = OWLClass(IRI(p, "B2"))
  val B3 = OWLClass(IRI(p, "B3"))
  val B4 = OWLClass(IRI(p, "B4"))
  val B5 = OWLClass(IRI(p, "B5"))
  val B6 = OWLClass(IRI(p, "B6"))
  val B7 = OWLClass(IRI(p, "B7"))
  val B8 = OWLClass(IRI(p, "B8"))
  val B9 = OWLClass(IRI(p, "B9"))
  val B10 = OWLClass(IRI(p, "B10"))
  val B11 = OWLClass(IRI(p, "B11"))
  val B12 = OWLClass(IRI(p, "B12"))
  val B13 = OWLClass(IRI(p, "B13"))
  val B14 = OWLClass(IRI(p, "B14"))
  val B15 = OWLClass(IRI(p, "B15"))
  val B16 = OWLClass(IRI(p, "B16"))
  val B17 = OWLClass(IRI(p, "B17"))
  val B18 = OWLClass(IRI(p, "B18"))
  val B19 = OWLClass(IRI(p, "B19"))
  val B20 = OWLClass(IRI(p, "B20"))
  val B21 = OWLClass(IRI(p, "B21"))
  val B22 = OWLClass(IRI(p, "B22"))
  val B23 = OWLClass(IRI(p, "B23"))
  val B24 = OWLClass(IRI(p, "B24"))
  val B25 = OWLClass(IRI(p, "B25"))
  val B26 = OWLClass(IRI(p, "B26"))
  val B27 = OWLClass(IRI(p, "B27"))
  val B28 = OWLClass(IRI(p, "B28"))
  val B29 = OWLClass(IRI(p, "B29"))
  val B30 = OWLClass(IRI(p, "B30"))
  val B31 = OWLClass(IRI(p, "B31"))

  val C1 = OWLClass(IRI(p, "C1"))
  val C2 = OWLClass(IRI(p, "C2"))
  val C3 = OWLClass(IRI(p, "C3"))
  val C4 = OWLClass(IRI(p, "C4"))
  val C5 = OWLClass(IRI(p, "C5"))
  val C6 = OWLClass(IRI(p, "C6"))
  val C7 = OWLClass(IRI(p, "C7"))
  val C8 = OWLClass(IRI(p, "C8"))
  val C9 = OWLClass(IRI(p, "C9"))
  val C10 = OWLClass(IRI(p, "C10"))
  val C11 = OWLClass(IRI(p, "C11"))
  val C12 = OWLClass(IRI(p, "C12"))
  val C13 = OWLClass(IRI(p, "C13"))
  val C14 = OWLClass(IRI(p, "C14"))
  val C15 = OWLClass(IRI(p, "C15"))
  val C16 = OWLClass(IRI(p, "C16"))
  val C17 = OWLClass(IRI(p, "C17"))
  val C18 = OWLClass(IRI(p, "C18"))

  // TODO: can these be localised to a single test?
  val son = ObjectProperty(IRI(p, "son"))
  val daughter = ObjectProperty(IRI(p, "daughter"))
  val child = ObjectProperty(IRI(p, "child"))
  val male = OWLClass(IRI(p, "male"))

  val F0 = ObjectProperty(IRI(p, "F0"))
  val invF0 = ObjectProperty(IRI(p, "invF0"))
  val F1 = ObjectProperty(IRI(p, "F1"))
  val invF1 = ObjectProperty(IRI(p, "invF1"))
  val F2 = ObjectProperty(IRI(p, "F2"))
  val F3 = ObjectProperty(IRI(p, "F3"))

  val (x, y) = (Term.x, Term.y)
  val (z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11) = (z(1), z(2), z(3), z(4), z(5), z(6), z(7), z(8), z(9), z(10), z(11))
  val (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10) = (f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10))

  // Implicits to allow us to use predicate names as IRIs without explicit conversion.
  implicit def toIRI(c: OWLClass): IRI = c.iri
  implicit def toIRI(c: ObjectProperty): IRI = c.iri

  // Convenience methods for constructing clauses.
  def Body(bs: Predicate*): Array[Predicate] = bs.toArray
  def Head(hs: Literal*): Array[Literal] = hs.toArray
  def OntologyClause(body: Array[Predicate], head: Array[Literal]) =
    com.sequoiareasoner.kernel.clauses.OntologyClause(body.toSeq, head.toSeq)

  val DoNothingLogger = new Logger {
    override def config(msg: String): Unit = {}
    override def trace(msg: String): Unit = {}
    override def info(msg: String): Unit = {}
    override def warn(msg: String): Unit = {}
    override def error(msg: String): Unit = {}
  }

  val DoNothingUnsupportedFeatureObserver = new UnsupportedFeatureObserverIgnore(DoNothingLogger)
}
