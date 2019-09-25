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

import org.scalatest.{FunSuite, BeforeAndAfter, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg.{DenseMatrix, DenseVector, sum}

@RunWith(classOf[JUnitRunner])
class ERGMTest extends FunSuite with BeforeAndAfter with Matchers {
  val file = getClass.getResource("/sample_edge_list.dnv").getPath
  val file2 = getClass.getResource("/sample_edge_list2.dnv").getPath
  val network = Network(file)
  val graph2 = Network(file2)
  val mat = network.adjacencyMatrix()
  val formula = Formula(mat, "edges", Map[String, Any](),
    network.nodes)
  val ergm = ERGM(formula)
  val matr = DenseMatrix((1.0, 0.0, 3.0), (0.0, 11.0, 12.0),
    (1.0, 2.0, 0.0))
  val erg = ERGM(Formula(matr, "edges", Map[String, Any](), Vector[Node]()))

  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
 }

 def replaceZeroes1(mat: DenseMatrix[Double], mat2: DenseMatrix[Double], rep: Double) = {
   (mat /:/ mat2).map(x => if (x.isNaN()) rep else x)
 }

 def replaceZeroes2(mat: DenseMatrix[Double], mat2: DenseMatrix[Double], rep: Double) = {
   mat(mat :== 0.0) := rep
   mat2(mat2 :== 0.0) := 1.0
   mat /:/ mat2
 }

  before {
    network.directed = true
    erg.directed = true
    erg.weighted = false
  }

  test ("Gets the Zero Matrix on Instantiation") {
    assert(ergm.empty == DenseMatrix.zeros[Double](9, 9))
  }

  test ("Creates unweighted from weighted network") {
    val expected = DenseMatrix((0.0, 0.0, 1.0), (0.0, 0.0, 1.0), (1.0, 1.0, 0.0))
    assert(erg.unweighted() == expected)
  }

  test ("Edge probability is equal to graph density") {
    val expected = 1.3
    val expected2 = 0.67
    val expected3 = 3.0
    val expected4 = 5.0
    erg.modelEdges() should equal (expected2 +- 0.05)
    erg.weighted = true
    erg.modelEdges() should equal (expected3 +- 0.05)
    erg.weighted = false
    erg.directed = false
    erg.modelEdges() should equal (expected +- 0.05)
  }

  test ("Make rand") {
    val item = sum((0 to 1000).map(x => {
      sum(erg.makeRandom())/((erg.network.cols) * (erg.network.cols - 1))
    }).toList) / 1000
    val item2 = sum((0 to 1000).map(x => {
      sum(ergm.makeRandom())/((ergm.network.cols) * (ergm.network.cols - 1))
    }).toList) / 1000
    item should equal (0.67 +- 0.02)
    item2 should equal (0.21 +- 0.05)
  }

  test ("Count mutual edges in graph") {
    val item = erg.mutualTies()
    val item2 = ergm.mutualTies()
    val expected = 2
    assert(item == expected)
    assert(item2 == expected)
    erg.directed = false
    assertThrows[ModelErrorException](erg.mutualTies())
  }

  test ("Diagonal") {
    val expected = DenseMatrix((0, 1, 1), (1, 0, 1), (1, 1, 0))
    assert(erg.diagonalMatrix() == expected)
  }

  test ("Simulate Degree Dist") {
    val expected = List(DenseVector(0, 1, 1))
    erg.simulateInDegreeDist()(2) should equal (1300 +- 150)
    erg.simulateInDegreeDist()(1) should equal (1300 +- 150)
    erg.simulateInDegreeDist()(0) should equal (300 +- 75)
  }

  test ("Model actor traits") {
    val expected = ergm.modelActorTraits((x) =>
      Option(x.attributes("AGE")) match {
        case Some("") => false
        case None => false
        case Some(x) => x.toInt <= "24".toInt })
    expected("with") should equal (0.066667 +- 0.0006)
    expected("in") should equal (0.4666 +- 0.002)
    expected("out") should equal (0.1333 +- 0.001)
  }
}
