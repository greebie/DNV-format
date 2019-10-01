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

import breeze.linalg.{DenseMatrix, DenseVector, *, sum, svd}
import breeze.numerics.sqrt
import breeze.plot._
import java.awt.{Paint, Color}

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
  private var _removeSingle: Boolean = true
  def removeSingle = _removeSingle
  def removeSingle_= (value: Boolean):Unit = _removeSingle = value
  val startMatrix: BivariateMatrix = createMatrix()
  // full case class with labels and abbrevs
  def bivariateMatrix: BivariateMatrix = if (removeSingle) {removeSingles()} else {startMatrix}
  def matrix: DenseMatrix[Double] = bivariateMatrix.matrix
  def rowLabels = bivariateMatrix.rowAbbrev
  def colLabels = bivariateMatrix.colAbbrev
  def sumMatrix = DenseMatrix.zeros[Double](matrix.rows, matrix.cols) := sum(matrix)
  // Number of observations.
  def n = sum(matrix)
  // Observed proportions.
  def P = matrix /:/ sumMatrix
  def massCols = sum(P(::, *))
  def massRows = sum(P(*, ::))
  // Expected proportions.
  def Expected = {
    val rows = massRows.map(x => {
      DenseVector((0 to massCols.t.length -1).map(y => {
        (massCols(y)*x)
      }).toArray)
    }).toArray
    DenseMatrix(rows:_*)
  }
  def Residuals = P - Expected
  // Indexed residuals.
  def I = Residuals /:/ Expected
  def ResidualsSS = Residuals.map(x => x * x) /:/ Expected
  def Chi = sum(ResidualsSS)
  // Standardized (normalized) residual.
  def Z = I *:* sqrt(Expected)
  val svd.SVD(u, s, v) = svd(Z)
  def standCoordsRow = u(*, ::).map(x => x /:/ sqrt(massRows))
  def standCoordsCol = v(*, ::).map(x => x /:/ sqrt(massCols.t))
  def rowCoords = standCoordsRow
  def colCoords = standCoordsCol
  def dimensions = s /:/ sum(s)
  def colProfile = matrix /:/ sum(massCols)
  def rowProfile = matrix /:/ sum(massRows)
  def avgColProfile = massRows /:/ n
  def avgRowProfile = massCols /:/ n


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
      val toId: Long = getIdAll(rows, atts(RowHeader)).getOrElse(-1)
      Edge(fromId, toId,
        Map("FROM" -> getNodeById(fromId, columns)
          .getOrElse(Node(-1, "", Map())).label,
          "TO" -> getNodeById(toId, rows)
          .getOrElse(Node(-1, "", Map())).label) ++
          atts.drop(2))})
      .filter(x => x.eto != -1 || x.efrom != -1 || x.attributes(ColumnHeader) != "" ||
        x.attributes(RowHeader) != "")
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

  def createMatrix() = {
    val edgesRow = edges.map(_.eto)
    val edgesCol = edges.map(_.efrom)
    val rs = rows.filter(x => edgesRow.contains(x.nid))
    val cls = columns.filter(x => edgesCol.contains(x.nid))
    val items = rs.map({case Node(rid, rlabel, ratts) =>
      val yes = edges.filter(x => x.eto == rid).map(_.efrom)
      new DenseVector[Double](cls.map({case Node(cid, clabel, catts) =>
        if (yes.contains(cid)) {1.0} else {0.0}
      }).toArray)
    }).toArray
    val rowLabels = rows.map(_.label)
    // TODO - if there's no "ABBREV" use "ID"
    val rowAbbrev = rows.map(x => {
      if (x.attributes.contains("ABBREV")) {
        x.attributes("ABBREV")
      } else if (x.attributes.contains("ABBREVIATION")) {
        x.attributes("ABBREVIATION")
      } else {
        x.label
      }
    })
    val colLabels = columns.map(_.label)
    // TODO - if there's no "ABBREV" use "ID"
    val colAbbrev = columns.map(x => {
      if (x.attributes.contains("ABBREV")) {
        x.attributes("ABBREV")
      } else if (x.attributes.contains("ABBREVIATION")) {
        x.attributes("ABBREVIATION")
      } else {
        x.label
      }})
   BivariateMatrix(rowLabels, rowAbbrev, colLabels, colAbbrev,
     DenseMatrix(items:_*))
  }

  def removeSingles() = {
    val matrix = startMatrix.matrix
    val sliceCol = (0 to matrix.cols -1)
      .filter(x => sum(matrix(::, x)) > 1).toSeq
    val sliceRow = (0 to matrix.rows -1)
      .filter(x => sum(matrix(x, ::)) >1).toSeq
    val rowL = startMatrix.rowLabels.filter(x => sliceRow
      .contains(startMatrix.rowLabels.indexOf(x)))
    val colL = startMatrix.colLabels.filter(x => sliceCol
      .contains(startMatrix.colLabels.indexOf(x)))
    val rowA = startMatrix.rowAbbrev.filter(x => sliceRow
      .contains(startMatrix.rowAbbrev.indexOf(x)))
    val colA = startMatrix.colAbbrev.filter(x => sliceCol
      .contains(startMatrix.colAbbrev.indexOf(x)))
    BivariateMatrix(
      rowL, rowA, colL, colA, matrix(sliceRow, sliceCol).toDenseMatrix)
  }

  def sizeItem(int: Int) = {
    5.0
  }

  def plotCoords() = {
    val whiteList = Vector[String]()
    val blackList = Vector[String]("Self-regulation", "NATO", "Voluntary Assess",
      "Conferences", "Procurement", "Protect Interests", "National Plan",
      "SME funding", "Task Force", "Academy", "Innovation Cent", "Intl Collab")
    val rLabels = Option(whiteList) match {
      case None => rowLabels.map(x => {
        if (blackList.contains(x)) {""} else {x}
      })
      case Some(y) if (y.isEmpty) => rowLabels.map(x => {
        print("BLACKLIST HERE")
        if (blackList.contains(x)) {""} else {x}
      })
      case Some(y) => rowLabels.map (x => {
        if (whiteList.contains(x)) {x} else {""}
      })
    }
    val f = Figure()
    f.height = 2250
    f.width = 2250
    val p = f.subplot(0)
    p += scatter(rowCoords(0, ::).t, rowCoords(1, ::).t,
      (x) => 0.05, (y)=> Color.BLUE, labels=rLabels)
    p += scatter(colCoords(0, ::).t, colCoords(1, ::).t, (x) => 0.2,
      (y) => Color.RED, labels=colLabels)
    p.title = "Correspondence Analysis of Cybersecurity Strategies by Country"
    p.xlabel = "Dimension 1 = " +
      (math.round(dimensions(0) * 10000) / 100).toString +
      "% Explanatory power"
    p.ylabel = "Dimension 2 = " +
      (math.round(dimensions(1) * 10000) / 100).toString +
      "% Explanatory power"

    val p2 = f.subplot(2, 1, 1)
    p2 += scatter(rowCoords(1, ::).t, rowCoords(2, ::).t, (x) => 0.05,
      (y) => Color.BLUE, labels=rLabels)
    p2 += scatter(colCoords(1, ::).t, colCoords(2, ::).t,
      (x) => 0.2, (y) => Color.RED, labels=colLabels)
    p2.title = "Correspondence Analysis of Cybersecurity Strategies by Country"
    p2.xlabel = "Dimension 2 = " +
      (math.round(dimensions(1) * 10000) / 100).toString +
      "% Explanatory power"
    p2.ylabel = "Dimension 3 = " +
      (math.round(dimensions(2) * 10000) / 100).toString +
      "% Explanatory power"

    val p3 = f.subplot(2, 2, 2)
    p3 += scatter(rowCoords(2, ::).t, rowCoords(3, ::).t, (x) => 0.05,
      (y) => Color.BLUE, labels=rLabels)
    p3 += scatter(colCoords(2, ::).t, colCoords(3, ::).t, (x) => 0.2,
      (y) => Color.RED, labels=colLabels)
    p3.title = "Correspondence Analysis of Cybersecurity Strategies by Country"
    p3.xlabel = "Dimension 3 = " +
      (math.round(dimensions(2) * 10000) / 100).toString +
      "% Explanatory power"
    p3.ylabel = "Dimension 4 = " +
      (math.round(dimensions(3) * 10000) / 100).toString +
      "% Explanatory power"
    val p4 = f.subplot(2, 2, 3)
    p4 += scatter(rowCoords(3, ::).t, rowCoords(4, ::).t, (x) => 0.05,
      (y) => Color.BLUE, labels=rLabels)
    p4 += scatter(colCoords(3, ::).t, colCoords(4, ::).t, (x) => 0.2,
      (y) => Color.RED, labels=colLabels)
    p4.title = "Correspondence Analysis of Cybersecurity Strategies by Country"
    p4.xlabel = "Dimension 4 = " +
      (math.round(dimensions(3) * 10000) / 100).toString +
      "% Explanatory power"
    p4.ylabel = "Dimension 5 = " +
      (math.round(dimensions(4) * 10000) / 100).toString +
      "% Explanatory power"
    f.saveas("cybersecurity.pdf", dpi=300)
  }
}

class BivariateImpl (val path: String) extends Bivariate  {

}

object Bivariate {
  def apply(path: String) = {
    new BivariateImpl(path)
  }
}
