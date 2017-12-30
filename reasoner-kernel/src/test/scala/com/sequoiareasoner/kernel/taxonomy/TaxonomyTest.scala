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

import com.sequoiareasoner.kernel.index.TotalIRIMultiMap
import org.scalatest.FunSuite
import com.sequoiareasoner.kernel.owl.iri.IRI

import scala.collection.mutable
import scala.collection.mutable.MultiMap
import scala.util.Random
import com.sequoiareasoner.arrayops._

object TaxonomyTest {
  private def toIRI(i: Int): IRI = IRI(s"test:$i")
  private def toInt(i: IRI): Int =
    if (i.isNothing) -1
    else if (i.isThing) -2
    else i.fullIriAsString.drop("test:".length).toInt

  private def matrixToString(matrix: Array[Array[Boolean]]): String = {
    val builder = new mutable.StringBuilder
    val dimension = matrix.length
    crange(0, dimension) { i =>
      val row = matrix(i)
      if (row contains true) {
        builder.append(i)
        builder.append(" -> [")
        var first = true
        crange(0, dimension) { j =>
          if (row(j)) {
            if (!first) builder.append(", ")
            first = false
            builder.append(j)
          }
        }
        builder.append("]\n")
      }
    }
    builder.result
  }

  private case class TaxonomyTestOutput(originalInput: String, components: Set[Set[Int]], edges: MultiMap[Int, Int])(outputName: String) {
    override def toString = {
      val builder = new mutable.StringBuilder
      builder ++= outputName
      builder += '\n'
      builder ++= originalInput
      builder ++= "--- EQUIVALENT IRIs ---\n"
      for (set <- components.toSeq.map{_.toSeq.sorted}.sortWith((xs,ys) => xs.head < ys.head))
        set.addString(builder, "[", ", ", "]\n")
      if (edges.keys.isEmpty) {
        builder ++= "--- NO EDGES ---\n"
      } else {
        val outVertices = edges.keys
        val keyWidth: Int = outVertices.max.toString.length
        val formatString = "%" + keyWidth + "s"
        builder ++= "--- EDGES ---\n"
        // Print vertices in order for easier comparison.
        for (u <- outVertices.toSeq.sorted) {
          val vs = edges(u)
          builder ++= formatString.format(u)
          builder ++= " -> "
          vs.toSeq.sorted.addString(builder, "[", ", ", "]\n")
        }
        builder ++= "-------------"
      }
      builder += '\n'
      builder.result
    }
  }

  private def convert(originalInput: String, taxonomy: Taxonomy): TaxonomyTestOutput = {
    val equivalents: mutable.Set[Set[Int]] = new mutable.HashSet[Set[Int]]
    val edges: MultiMap[Int, Int] = new mutable.HashMap[Int, mutable.Set[Int]] with MultiMap[Int, Int]
    def f1(component: ImmutableIRISet): Unit =
      if (!component.representative.isNothing && !component.representative.isThing)
        equivalents += component.iterator.map(toInt).toSet
    def f2(sub: IRI, sup: IRI): Unit =
      if (!sub.isNothing && !sup.isThing)
        edges.addBinding(toInt(sub), toInt(sup))
    taxonomy.foreach(f1, f2)
    // Map each IRI contained in an equivalent class to its canonical representative to allow for direct comparison.
    val canonicalRepresentative = new mutable.HashMap[Int, Int]
    for (component <- equivalents; min = component.min; elem <- component)
      canonicalRepresentative += elem -> min
    val edgesOnRepresentative = new mutable.HashMap[Int, mutable.Set[Int]] with MultiMap[Int, Int]
    for ((sub, supers) <- edges; sup <- supers)
      edgesOnRepresentative.addBinding(canonicalRepresentative(sub), canonicalRepresentative(sup))
    TaxonomyTestOutput(originalInput, equivalents.toSet, edgesOnRepresentative)("Actual Output")
  }

}

/** Test to check that the optimised transitive reduction implementation in the reasoner is correct.
  *
  * The optimised implementation is compared against a known correct algorithm.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class TaxonomyTest extends FunSuite {
  import TaxonomyTest._

  def testCase(name: String, numberOfNodes: Int)(matrixEntry: (Int, Int) => Boolean): Unit = {
    test(name) {
      val matrix = Array.tabulate[Boolean](numberOfNodes, numberOfNodes)(matrixEntry)
      val originalInput = matrixToString(matrix)
      // Returns true if i has an incoming or outgoing edge.
      def hasIncident(k: Int): Boolean = ccontains(matrix(k), true) || !cforall(0, numberOfNodes){ i => !matrix(i)(k) } // TODO: replace with exists.
      // Brute force transitive closure (Floyd-Warshall algorithm).
      crange (0, numberOfNodes) { k =>
        crange (0, numberOfNodes) { i =>
          if (matrix(i)(k))
            crange (0, numberOfNodes) { j =>
              if (matrix(k)(j)) matrix(i)(j) = true
            }
        }
      }
      // Brute force connected components.
      // Only build the components for nodes that have an incident edge because the reasoner taxonomy construction only works with edges.
      val visited = new mutable.BitSet
      val connectedComponents = new mutable.LongMap[Set[Int]]
      crange (0, numberOfNodes) { i =>
        if (visited.add(i) && hasIncident(i)) {
          // Build connected component containing i.
          val component = new mutable.BitSet
          component += i
          crange (0, numberOfNodes) { j =>
            if (i != j && matrix(i)(j) && matrix(j)(i)) {
              visited += j
              component += j
            }
          }
          connectedComponents.put(i, component.toSet)
        }
      }
      // Construct expected output.
      // First, construct the path matrix for the DAG of connected components.
      val pathDAGMatrix = Array.tabulate[Boolean](numberOfNodes, numberOfNodes) {
        // This is correct because matrix is a path matrix of the original graph.
        // Self-loops introduced by transitive closure will break Harry Hsu algorithm which requires a DAG.
        case (i: Int, j: Int) => i != j && matrix(i)(j) && (connectedComponents contains i) && (connectedComponents contains j)
      }
      // Finally, brute-force transitive reduction of DAG (Harry Hsu algorithm, requires path matrix).
      crange (0, numberOfNodes) { k =>
        crange (0, numberOfNodes) { i =>
          if (pathDAGMatrix(i)(k))
            crange (0, numberOfNodes) { j =>
              if (pathDAGMatrix(k)(j))
                pathDAGMatrix(i)(j) = false
            }
        }
      }
      // Convert expected to adjacency list.
      val expectedEdges: MultiMap[Int, Int] = new mutable.HashMap[Int, mutable.Set[Int]] with MultiMap[Int, Int]
      crange (0, numberOfNodes) { i =>
        crange(0, numberOfNodes) { j =>
          if (pathDAGMatrix(i)(j)) expectedEdges.addBinding(i, j)
        }
      }
      val expected = TaxonomyTestOutput(originalInput, connectedComponents.values.toSet, expectedEdges)("Expected output")
      // Construct the taxonomy input using generated IRIs.
      val input = new TotalIRIMultiMap[IRI](new Array[IRI](_))
      crange (0, numberOfNodes) { i =>
        crange (0, numberOfNodes) { j =>
          if (matrix(i)(j))
            input.addBinding(toIRI(i), toIRI(j))
        }
      }
      // Compute actual taxonomy output.
      val actual: Taxonomy = Taxonomy(input)
      assert(convert(originalInput, actual) === expected)
    }
  }

  testCase(name = "Saved test 1", numberOfNodes = 30) {
    (i, j) =>
      (i == 4 && j == 19) ||
      (i == 5 && j == 16) ||
      (i == 8 && j == 13) ||
      (i == 8 && j == 24) ||
      (i == 10 && j == 15) ||
      (i == 12 && j == 14) ||
      (i == 14 && j == 18) ||
      (i == 16 && j == 14) ||
      (i == 19 && j == 0) ||
      (i == 20 && j == 3) ||
      (i == 20 && j == 18) ||
      (i == 24 && j == 6) ||
      (i == 24 && j == 8) ||
      (i == 24 && j == 25) ||
      (i == 27 && j == 26)
  }

  testCase(name = "Saved test 2", numberOfNodes = 30) {
    (i, j) =>
      (i == 7 && j == 27) ||
      (i == 25 && j == 4) ||
      (i == 27 && j == 25)
  }

  testCase(name = "Saved test 3", numberOfNodes = 30) {
    (i, j) =>
      (i == 1 && j == 2) ||
      (i == 5 && j == 15) ||
      (i == 7 && j == 8) ||
      (i == 12 && j == 24) ||
      (i == 13 && j == 2) ||
      (i == 13 && j == 17) ||
      (i == 15 && j == 21) ||
      (i == 17 && j == 18) ||
      (i == 18 && j == 19) ||
      (i == 20 && j == 24) ||
      (i == 21 && j == 15) ||
      (i == 22 && j == 6) ||
      (i == 22 && j == 11) ||
      (i == 23 && j == 2)
  }

  testCase(name = "Saved taxonomy test 4", numberOfNodes = 2) {
    (i, j) =>
      (i == 0 && j == 1) || (i == 1 && j == 0)
  }

  // These are the two parameters to vary for test coverage.
  val sizes = Seq(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000,2000)
  val densities = Seq(0.00001,0.00003,0.00005,0.0001,0.0005,0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008,0.009,0.01,
                      0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,
                      0.8,0.9,0.95)

  private[this] var counter = 0
  for (size <- sizes; density <- densities) {
    testCase(name = s"Random taxonomy test $counter [size == $size, density = $density]", numberOfNodes = size) {
      (i, j) => i != j && Random.nextDouble < density
    }
    counter += 1
  }

}
