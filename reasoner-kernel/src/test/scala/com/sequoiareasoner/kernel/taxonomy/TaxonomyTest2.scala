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

package com.sequoiareasoner.kernel.taxonomy

import org.scalatest.FunSuite
import com.sequoiareasoner.kernel.index.{ArrayBuilders, TotalIRIMultiMap}
import com.sequoiareasoner.kernel.owl.iri.{IRI, Prefix}

import scala.collection.mutable

object TaxonomyTest2 {
  val p = Prefix(":", IRI("http://www.test.com/schema#"))
  val A = IRI(p, "A")
  val B = IRI(p, "B")
  val C = IRI(p, "C")
  val D = IRI(p, "D")
  val E = IRI(p, "E")
  val F = IRI(p, "F")
  val G = IRI(p, "G")
  val H = IRI(p, "H")
  val I = IRI(p, "I")
  val Nothing = IRI("http://www.w3.org/2002/07/owl#Nothing")
  val Thing = IRI("http://www.w3.org/2002/07/owl#Thing")
  val Animal = IRI(p, "Animal")
  val Beast = IRI(p, "Beast")
  val Human = IRI(p, "Human")
  val Irrational = IRI(p, "Irrational")
  val Kangaroo = IRI(p, "Kangaroo")
  val KangarooInfant = IRI(p, "KangarooInfant")
  val MaternityKangaroo = IRI(p, "MaternityKangaroo")
  val Parent = IRI(p, "Parent")
  val Pouch = IRI(p, "Pouch")
  val Rational = IRI(p, "Rational")

  private final class DecoratedSet[A](val set: Set[A]) {
    override def equals(o: Any): Boolean = o match {
      case that: DecoratedSet[_] => this.set == that.set
      case _                     => false
    }
    override def toString: String = set.toSeq.map{_.toString}.sorted.mkString("\n","\n","\n")
  }

  private def decorate[A](set: Set[A]): DecoratedSet[A] = new DecoratedSet(set)

}

class TaxonomyTest2 extends FunSuite {
  import TaxonomyTest2._

  // TODO: If map is empty, connect Nothing to Thing if input map is empty!
  // TODO: need to connect each node to Thing if no super concepts.

  // TODO: need to add tests on the direct and indirect super and sub classes of concepts.
  // For example, need to test that owl:Nothing is a subclass of each concept, and that owl:Thing is a superclass of each concept.
  // This will test if the taxonomy has been wired up correctly. Can also count the number of sub and super concepts of a class.

  private[this] def computeTaxonomy(input: Set[(IRI, IRI)]): (Set[Set[IRI]], Set[(IRI, IRI)]) = {
    val map = new TotalIRIMultiMap[IRI](ArrayBuilders.iriArrayBuilder)
    for ((sub, sup) <- input)
      map.addBinding(sub, sup)
    val taxonomy = Taxonomy(map)
    val expectedEquivalent = new mutable.HashSet[Set[IRI]]
    val expectedSubClass = new mutable.HashSet[(IRI, IRI)]
    def buildEquivalent(set: ImmutableIRISet): Unit = expectedEquivalent += set.iterator.toSet
    def buildSubClass(sub: IRI, sup: IRI): Unit = expectedSubClass += ((sub, sup))
    taxonomy.foreach(buildEquivalent, buildSubClass)
    (expectedEquivalent.toSet, expectedSubClass.toSet)
  }

  private[this] def runTest(input: Set[(IRI, IRI)],
                            expectedEquivalent: Set[Set[IRI]],
                            expectedSubClass: Set[(IRI, IRI)]): Unit = {
    val (actualEquivalent, actualSubClass) = computeTaxonomy(input)
    assert(decorate(actualEquivalent) === decorate(expectedEquivalent))
    assert(decorate(actualSubClass) === decorate(expectedSubClass))
  }

  test("Handling of owl:Thing"){
    // This test checks that no redundant taxonomy axioms are generated (e.g., owl:Thing -> owl:Thing).
    val input = Set(
      (Thing, A)
    )
    val expectedEquivalent = Set(
      Set(Thing, A),
      Set(Nothing),
    )
    val expectedSubClass = Set(
      (Nothing, Thing)
    )
    runTest(input, expectedEquivalent, expectedSubClass)
  }

  test ("basic test") {
    val input = Set(
      (B, D), (B, A),
      (C, B), (C, D), (C, A)
    )
    val expectedEquivalent = Set(
      Set(A), Set(B), Set(C), Set(D), Set(Nothing), Set(Thing)
    )
    val expectedSubClass = Set(
      (B, A), (B, D), (C, B), (Nothing, C), (A, Thing), (D, Thing)
    )
    runTest(input, expectedEquivalent, expectedSubClass)
  }

  test("transitive closure reduction") {
    /* We send the (transitively closed) subsumptions to the taxonomy and expect it to return the
     * transitively reduced taxonomy pictured below.
     *
     *       \top
     *      /  |  \
     *     /   |   \
     *    /    |    \
     *  {A}   {B}   {C,D}
     *   |     \     /
     * {E,F}    \   /
     *     \     \ /
     *      \     |
     *       \    |
     *        \   |
     *         {G,H,I}
     *           |
     *           |
     *           |
     *         \bot
     */
    val input = Set(
      // C == D
      (C, D), (D, C),
      // E == F
      (E, F), (F, E),
      // E OR F -> A
      (E, A), (F, A),
      // G == H == I
      (G, H), (G, I),
      (H, I), (H, G),
      (I, G), (I, H),
      // G OR H OR I -> E OR F OR A
      (G, E), (G, F), (G, A),
      (H, E), (H, F), (H, A),
      (I, E), (I, F), (I, A),
      // G OR H OR I -> B OR C OR D
      (G, B), (G, C), (G, D),
      (H, B), (H, C), (H, D),
      (I, B), (I, C), (I, D),
    )
    val expectedEquivalent = Set(
      Set(A), Set(B), Set(C, D), Set(E, F), Set(G, H, I), Set(Nothing), Set(Thing)
    )
    val expectedSubClass = Set(
      (E, A), (G, E), (G, B), (G, C), (A, Thing), (B, Thing), (C, Thing), (Nothing, G)
    )
    runTest(input, expectedEquivalent, expectedSubClass)
  }

  test("kangaroo taxonomy test") {
    val input = Set(
      (Beast, Irrational),
      (Beast, Animal),
      (Kangaroo, Beast),
      (Kangaroo, Irrational),
      (Kangaroo, Animal),
      (Human, Rational),
      (Human, Animal),
      (KangarooInfant, Kangaroo),
      (KangarooInfant, Beast),
      (KangarooInfant, Irrational),
      (KangarooInfant, Animal),
      (Parent, Human),
      (Parent, Rational),
      (Parent, Animal),
      (MaternityKangaroo, Kangaroo),
      (MaternityKangaroo, Human),
      (MaternityKangaroo, Parent),
      (MaternityKangaroo, Beast),
      (MaternityKangaroo, Rational),
      (MaternityKangaroo, Animal),
      (MaternityKangaroo, Irrational),
      (MaternityKangaroo, Nothing),
    )
    val expectedEquivalent = Set(
      Set(Nothing, MaternityKangaroo),
      Set(Animal),
      Set(Beast),
      Set(Human),
      Set(Irrational),
      Set(Kangaroo),
      Set(KangarooInfant),
      Set(Parent),
      Set(Rational),
      Set(Thing),
    )
    val expectedSubClass = Set(
      (Beast, Animal),
      (Beast, Irrational),
      (Human, Animal),
      (Human, Rational),
      (Kangaroo, Beast),
      (KangarooInfant, Kangaroo),
      (Parent, Human),
      (Irrational, Thing),
      (Animal, Thing),
      (Rational, Thing),
      (Nothing, KangarooInfant),
      (Nothing, Parent),
    )
    runTest(input, expectedEquivalent, expectedSubClass)
  }

}
