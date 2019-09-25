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

import breeze.linalg.{DenseMatrix, sum, trace, diag, *, argsort,
  lowerTriangular, upperTriangular}
import breeze.linalg.support.CanSlice

trait ERGM {
  val form: Formula
  val atts: Vector[Node] = form.atts
  val diagonal = diagonalMatrix()
  val network: DenseMatrix[Double] = form.network *:* diagonal
  val empty: DenseMatrix[Double]
  var weighted = false
  var directed = true

  /** Converts network to an unweighted network **/
  def unweighted(): DenseMatrix[Double] = {
    (network /:/ network).map(x => if (x.isNaN()) 0.0 else x)
  }

  /** Estimates the probability of an edge in the network (equal to density) **/
  def modelEdges() = {
    val net = if (weighted) { network } else { unweighted() }
    val pot = if (directed) { net.cols * (net.cols - 1) }
      else { (net.cols * (net.cols -1)) / 2 }
    sum(net)/pot
  }

  def mutualTies() = {
    if (!directed) {
      throw new ModelErrorException ("Mutual ties are only appropriate for " +
        "directed graphs.")
    }
    var n = 0
    var count = 0
    while (n < unweighted().cols) {
      var k = n + 1
      while (k < unweighted().cols) {
        if (unweighted()(n, k) >= 1 && unweighted()(k, n) >= 1) {
          count += 1
        }
        k += 1
      }
      n += 1
    }
    count
  }

  //* Model for an attribute att has value expect *//
  def modelActorTraits(att: Node => Boolean) = {
    val attTrue = atts.filter(att).map(x => x.nid.toInt)
    val attFalse = atts.filterNot(att).map(x => x.nid.toInt)
    val homophily = attTrue.flatMap(x => attTrue.map( y => (x, y)))
    val heterophily = attTrue.flatMap(x => attFalse.map( y => (x, y)))
    val in = sum(heterophily.map({case (x: Int, y: Int) => network(x, y)}))
    val out = sum(heterophily.map({case (x: Int, y: Int) => network(y, x)}))
    val homo = sum(homophily.map({case (x: Int, y: Int) => network(x, y)}))
    val nw = sum(network)
    if (nw == 0.0) {
      throw new ModelErrorException ("Cannot model actor" +
        "traits on an empty network") }
    Map("with" -> homo/nw, "in" -> in/nw, "out" -> out/nw)
  }

  def modelSample() = {

  }

  def randomNetwork() = {
    DenseMatrix.rand(network.rows, network.cols)
  }

  def makeRandom(): DenseMatrix[Double] = {
    val prob = modelEdges()
    val rand = randomNetwork.map(x => if (x >= prob) {0.0} else {1.0})
    rand *:* diagonalMatrix()
  }

  def diagonalMatrix() = {
    val x = form.network.cols
    DenseMatrix.tabulate(x, x){case (i, j) => if (i == j) {0.0} else {1.0}}
  }

  def simulateInDegreeDist(repeats: Int = 1000) = {
    val prob = modelEdges()
    (0 to repeats)
      .map(x => makeRandom()(*, ::).map(x => sum(x)).toArray)
      .toList
      .flatten
      .groupBy(identity).mapValues(_.size)
  }
 }

class ERGMImpl (val form: Formula) extends ERGM {
  val empty = DenseMatrix.zeros[Double](form.network.cols, form.network.rows)
}

object ERGM {
  def apply(form: Formula) = {
    new ERGMImpl(form)
  }
}
