/*
 * Copyright 2018 Ryan Deschamps
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 * may be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.dnvparser

import scala.io.Source
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg.{DenseVector, DenseMatrix}

@RunWith(classOf[JUnitRunner])
class NetworkTest extends FunSuite with BeforeAndAfter {
  val file = getClass.getResource("/sample_edge_list.dnv").getPath
  val file2 = getClass.getResource("/sample_edge_list2.dnv").getPath
  val network = Network(file)
  val bob = DenseVector(1.0,1.0,1.0,0.0,0.0,0.0,0.0,0.0,0.0)
  val george = DenseVector(0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0)

  before {
    network.directed = false
  }

  test ("Get neighbours of Node") {
    val nodeIdent: String = "Bob Bobblewot"
    val nodeIdent2: String = "George"
    val nodeIdent3: String = "Fake"
    assert(network.outNeighbors(nodeIdent3) == Vector())
    assert(network.inNeighbors(nodeIdent3) == Vector())
    assert(network.outNeighbors(nodeIdent).map(_.nid) == Vector(1, 2, 0))
    assert(network.inNeighbors(nodeIdent).map(_.nid) == Vector(0, 1, 2, 3))
    assert(network.neighbors(nodeIdent).map(_.nid) == Vector(1, 2, 0, 3))
    assert(network.outNeighbors(nodeIdent2).map(_.nid) == Vector(4, 6, 7))
    assert(network.inNeighbors(nodeIdent2).map(_.nid) == Vector())
    assert(network.neighbors(nodeIdent2).map(_.nid) == Vector(4, 6, 7))
    network.directed = true
    assert(network.neighbors(nodeIdent).map(_.nid) == Vector(1, 2, 0,
      0, 1, 2, 3))
  }

  test ("Get the nodeset") {
    val nodeset = network.nodeSet()
    assert(nodeset.map(_._1) == Vector(0,1,2,3,4,5,6,7,8))
    assert(nodeset.map(_._2) == Vector("Bob Bobblewot", "Sandy Poland",
      "Anderson Li", "4", "8", "George", "9", "10", "6"))
  }

  test ("Get the matrix rows") {
    val matrixRow1 = network.neighborsVector("Bob Bobblewot")
    val matrixRow2 = network.neighborsVector("George")
    assert(matrixRow1 == bob)
    assert(matrixRow2 == george)
  }

  test ("Get the Adjacency Matrix") {
    val matrix = network.adjacencyMatrix()
    assert(matrix.t(::, 0) == bob)
    assert(matrix.t(::, 4) == george)
  }

  test("Degree Matrix") {
    val matrix = network.degreeMatrix()
    val in = network.degreeMatrix("in")
    val out = network.degreeMatrix("out")
    assert(matrix == DenseMatrix(Array(4,  2,  3,  3,  5,  3,  3,  3,  3)))
    assert(out == DenseMatrix(Array(3,  1,  2,  3,  3,  3,  0,  0,  3)))
    assert(in == DenseMatrix(Array(4,  2,  3,  0,  3,  0,  3,  3,  0)))
    network.directed = true
    val newMatrix = network.degreeMatrix()
    assert(newMatrix == DenseMatrix(Array(7, 3, 5, 3, 6, 3, 3, 3, 3)))
  }

  test("Eigenvector Degree") {
    val eig = network.normalizeValues(network.eigenVectorCentrality())
    assert(eig.map(x => "%.2f".format(x).toDouble).t == DenseMatrix(Array(0.0,
      0.24, 0.47, 0.38, 0.12, 0.81, 0.56, 0.56, 1.0)))
  }
}
