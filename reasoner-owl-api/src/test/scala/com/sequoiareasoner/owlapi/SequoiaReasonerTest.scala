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

package com.sequoiareasoner.owlapi

import org.scalatest.FunSuite
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model.{AddImport, IRI, OWLAxiom, OWLOntology, RemoveImport}
import org.semanticweb.owlapi.util.DefaultPrefixManager
import uk.ac.manchester.cs.owl.owlapi.OWLImportsDeclarationImpl

/**
  * Testing correctness of the implementation of {@link OWLReasoner} interface.
  */
class SequoiaReasonerTest extends FunSuite {
  import OWLAPITestUtils.{loadOntology, getAxioms}

  /**
    * Testing correctness of the reasoner with respect to ontology changes.
    */
  ignore ("changes") { // FIXME: flushing is not yet supported.
    val man = OWLManager.createOWLOntologyManager
    val dataFactory = man.getOWLDataFactory
    val pm = new DefaultPrefixManager
    pm.setDefaultPrefix("http://www.example.com/main#")
    pm.setPrefix("A:", "http://www.example.com/A#")
    pm.setPrefix("B:", "http://www.example.com/B#")
    val mainX = dataFactory.getOWLClass(":X", pm)
    val mainY = dataFactory.getOWLClass(":Y", pm)
    val extA = dataFactory.getOWLClass("A:A", pm)
    val extB = dataFactory.getOWLClass("B:B", pm)
    val extC = dataFactory.getOWLClass("B:C", pm)
    val root: OWLOntology = loadOntology(man, "ontologies", "root.owl")
    val factory = SequoiaReasonerFactory.getInstance
    val reasoner = factory.createReasoner(root)
    try {
      assert(root.getAxiomCount === 3)
      assert(root.getImportsClosure.size === 3)
      assert(getAxioms(root).size === 6)
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(mainY))
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(extA))
      assert(reasoner.getSuperClasses(mainY, true).containsEntity(extB))
      assert(reasoner.getSuperClasses(extA, true).containsEntity(extB))
      assert(reasoner.getSuperClasses(extB, true).containsEntity(extC))
      val axiom1 = dataFactory.getOWLSubClassOfAxiom(mainX, mainY)
      man.removeAxiom(root, axiom1)
      reasoner.flush
      assert(root.getAxiomCount === 2)
      assert(root.getImportsClosure.size === 3)
      assert(getAxioms(root).size === 5)
      assert(!reasoner.getSuperClasses(mainX, true).containsEntity(mainY))
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(extA))
      assert(reasoner.getSuperClasses(mainY, true).containsEntity(extB))
      assert(reasoner.getSuperClasses(extA, true).containsEntity(extB))
      assert(reasoner.getSuperClasses(extB, true).containsEntity(extC))
      man.addAxiom(root, axiom1)
      val axiom2 = dataFactory.getOWLSubClassOfAxiom(extA, extB)
      man.removeAxiom(root, axiom2)
      reasoner.flush
      assert(root.getAxiomCount === 3)
      assert(root.getImportsClosure.size === 3)
      assert(getAxioms(root).size === 6)
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(mainY))
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(extA))
      assert(reasoner.getSuperClasses(mainY, true).containsEntity(extB))
      assert(reasoner.getSuperClasses(extA, true).containsEntity(extB))
      assert(reasoner.getSuperClasses(extB, true).containsEntity(extC))
      val importA = new OWLImportsDeclarationImpl(IRI.create("/impA"))
      val changeRemove = new RemoveImport(root, importA)
      man.applyChange(changeRemove)
      reasoner.flush
      assert(root.getAxiomCount === 3)
      assert(root.getImportsClosure.size === 1)
      assert(getAxioms(root).size === 3)
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(mainY))
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(extA))
      assert(reasoner.getSuperClasses(mainY, true).containsEntity(extB))
      assert(!reasoner.getSuperClasses(extA, true).containsEntity(extB))
      assert(!reasoner.getSuperClasses(extB, true).containsEntity(extC))
      val importB = new OWLImportsDeclarationImpl(IRI.create("/impB"))
      val changeAdd = new AddImport(root, importB)
      man.applyChange(changeAdd)
      val axiom3 = dataFactory.getOWLSubClassOfAxiom(mainY, extB)
      man.removeAxiom(root, axiom3)
      reasoner.flush
      assert(root.getAxiomCount === 2)
      assert(root.getImportsClosure.size === 2)
      assert(getAxioms(root).size === 4)
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(mainY))
      assert(reasoner.getSuperClasses(mainX, true).containsEntity(extA))
      assert(!reasoner.getSuperClasses(mainY, true).containsEntity(extB))
      assert(!reasoner.getSuperClasses(extA, true).containsEntity(extB))
      assert(reasoner.getSuperClasses(extB, true).containsEntity(extC))
    } finally {
      reasoner.dispose
    }
  }

}
