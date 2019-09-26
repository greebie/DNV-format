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

trait Bivariate extends GraphReader {
  val DimKey = "NODESET"
  val Cols = "COL"
  val Rows = "ROW"
  val ColumnHeader = "FROM"
  val RowHeader = "TO"
  override val nodesReader: Vector[Node] = getNodes()
  override val edgesReader: Vector[Edge] = getEdges()
  val nodes: Vector[Node] = getAllNodes()
  val columns: Vector[Node] = getColumnNodes()
  val rows: Vector[Node] = getRowNodes()
  val edges: Vector[Edge] = getAllEdges()

  def getColumnNodes(): Vector[Node] = {
    nodes.filter(x => x.attributes(DimKey) == Cols)
  }

  def getRowNodes(): Vector[Node] = {
    nodes.filter(x => x.attributes(DimKey) == Rows)
  }

  def getColumnReaderNodes(): Vector[Node] = {
    nodesReader.filter(x => x.attributes(DimKey) == Cols)
  }
  def getRowReaderNodes(): Vector[Node] = {
    nodesReader.filter(x => x.attributes(DimKey) == Rows)
  }
  override def getNodes(): Vector[Node] = {
    getBivariateNodes()
  }

  def getNodesFromEdges(nodeVect: Vector[Node],
    key: String="FROM", nodeset: String="ROW"): Vector[Node] = {
    val hd: Iterable[String] = nodeVect.map(_.attributes.keySet).head
    val start =  maxNodeId(nodeVect).toLong + 1
    val edges = edgesReader
      .filter(edge => getId(edge.attributes(key)) == None)
      .map(edge =>
          // if there is no node for an edge, keep the edge name for now.
          // then flatten them into a list of nodes with no Node object.
          getId(edge.attributes(key)) match {
            case None => edge.attributes(key)
            case Some(x) => -1 })
      .filter(node => node != -1)
      .distinct
      .zipWithIndex.map({ case (n, ind) => (n, ind + start) })
      .map(n => Node(n._2.toLong, n._1.toString,
        hd.map(x => x match {
          case "ID" => (x, n._1.toString)
          case "LABEL" => (x, n._1.toString)
          case "NODESET" => (x, nodeset)
          case _ => (x, "")
        }).toMap)).toVector
      (nodeVect ++ edges).distinct
  }

  def getAllNodes(): Vector[Node] = {
    val rows = getRowReaderNodes()
    val cols = getColumnReaderNodes()
    val newRows = getNodesFromEdges(rows, RowHeader, "ROW")
    val newCols = getNodesFromEdges(cols, ColumnHeader, "COL")
    newRows ++ newCols
  }

  def getAllEdges(): Vector[Edge] = {
    edgesReader.map({ case Edge(efrom, eto, atts) =>
      val fromId: Long = getIdAll(columns, atts(ColumnHeader)).getOrElse(-1)
      val toId: Long = getIdAll(rows, atts(ColumnHeader)).getOrElse(-1)
      Edge(fromId, toId,
        Map("FROM" -> getNodeById(fromId, columns)
          .getOrElse(Node(-1, "", Map())).label,
          "TO" -> getNodeById(toId, rows)
          .getOrElse(Node(-1, "", Map())).label) ++
          atts.drop(2))})
      .filter(x => x.eto != -1 || x.efrom != -1 || x.attributes(ColumnHeader) != "" ||
        x.attributes(ColumnHeader) != "")
  }

  def getNodeById(nodeId: Long, nds: Vector[Node] = columns) = {
    nds.filter(x => x.nid == nodeId).headOption
  }

  def getIdAll(nodex: Vector[Node] = columns, ident: String,
    alt: Option[String] = None): Option[Long] = {
    if (nodex.map(x => x.attributes("ID")).contains(ident)) {
      Some(nodex.filter(x => x.attributes("ID") == ident)
        .map(x => x.nid).head)
    } else if (nodex.map(x => x.attributes("LABEL")).contains(ident)) {
      Some(nodex.filter(x => x.attributes("LABEL") == ident)
        .map(x => x.nid).head)
    } else {
      alt match {
        case Some(att) => Some(nodex.filter(x => {
          stringToArray(x.attributes(att)).getOrElse(List[String]())
          .contains(ident) })
          .map(x => x.nid).head)
        case _ => None
      }
    }
  }
}

class BivariateImpl (val path: String) extends Bivariate  {

}

object Bivariate {
  def apply(path: String) = {
    new BivariateImpl(path)
  }
}
