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

 package org.dmvparser

 import breeze.linalg.{DenseVector, DenseMatrix}
 import breeze.linalg.{eig, min, max}
 import breeze.linalg.operators.OpMulInner._

 trait Network extends Graph {
   val matrix = adjacencyMatrix()

   def nodeSet(): Vector[(Long, String)] = {
     nodes.map(x => (x.nid, x.label)).distinct
   }

   def outNeighbors (nodeIdent: String): Vector[Node] = {
     edges.filter(x => x.efrom == getId(nodeIdent, all_nodes=true).getOrElse(-1))
       .map(x => getNodeById(x.eto) match {
         case Some(node) => node
         case None => Node()
       }).distinct
   }

   def inNeighbors(nodeIdent: String): Vector[Node] = {
     edges.filter(x => x.eto == getId(nodeIdent, all_nodes=true).getOrElse(-1))
       .map(x => getNodeById(x.efrom) match {
         case Some(node) => node
         case None => Node()
       }).distinct
   }

   def neighborsVector(nodeIdent: String,
     weighted: Boolean = false): DenseVector[Double] = {
       // weighted logic here.
     val neighbors = outNeighbors(nodeIdent)
     val nodeset = (nodeSet()
       .map({case (id, label) => if (neighbors.map(_.nid).contains(id)) { 1.0 }
         else { 0.0 }})).toArray
     DenseVector(nodeset)
   }

   def neighbors(nodeIdent: String): Vector[Node] = {
     if (directed) {
       outNeighbors(nodeIdent) ++ inNeighbors(nodeIdent)
     } else {
       (outNeighbors(nodeIdent) ++ inNeighbors(nodeIdent)).distinct }
   }

   def degreeMatrix(degType:String = "all"): DenseMatrix[Double] = {
     val degrees = nodeSet().map({case (id, label) => degType match {
       case "in" => inNeighbors(label).length.toDouble
       case "out" => outNeighbors(label).length.toDouble
       case "all" => neighbors(label).length.toDouble
     }}).toArray
     DenseMatrix(degrees)
   }

   def adjacencyMatrix(): DenseMatrix[Double] = {
     val vectors = nodeSet().map({case (id, label) =>
       neighborsVector(label).toArray})
     DenseMatrix(vectors:_*)
   }

   def eigenvectorMatrix(): DenseMatrix[Double] = {
     eig(matrix).eigenvectors
   }

   def normalizeValues(vals: DenseMatrix[Double]) = {
     val minimum = min(vals)
     val maximum = max(vals)
     vals.map( x => ((x - minimum) / (maximum - minimum)))
   }

   def eigenVectorCentrality() = {
     val values = eigenvectorMatrix()
     val degrees = degreeMatrix()
     values * degrees.t
   }
 }

 class NetworkImpl (val path: String) extends Network {

 }

 object Network {
   def apply(path: String) = {
     new NetworkImpl(path)
   }
 }
