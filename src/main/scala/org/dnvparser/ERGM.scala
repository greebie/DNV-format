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

import breeze.linalg.{DenseMatrix, sum, trace, diag}

trait ERGM {
  val empty: DenseMatrix[Double]
  val form: Formula
  val network: DenseMatrix[Double] = form.network
  var simple = true
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
    val simp = if (simple) {(sum(net) - trace(net))/ pot}
      else {sum(net)/pot}
    simp
  }

  def randomNetwork() = {
    DenseMatrix.rand(network.rows, network.cols)
  }

  def makeRandom() = {
    val prob = modelEdges()
    val rand = randomNetwork.map(x => if (x >= prob) {0.0} else {1.0})
    rand *:* diagonalMatrix()
  }

  def diagonalMatrix() = {
    val x = empty.cols
    DenseMatrix.tabulate(x, x){case (i, j) => if (i == j) {0.0} else {1.0}}
  }

  def simulate() = {
    val prob = modelEdges()
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
