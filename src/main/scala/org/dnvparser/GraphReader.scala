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
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import scala.math.max

trait GraphReader {
  def path: String // path to file
  def source = getSource(path)
  val logger = Logger(LoggerFactory.getLogger("DNVParser Graph Functions"))

  //* Default values */
  val Command: Char = '>'
  val SplitChar: String = "="
  val GraphStart: String = ">GRAPH"
  val NodeStart: String = ">NODES"
  val EdgeStart: String = ">EDGES"
  val BivariateKey: String = "BIVARIATE"
  val DirectedKey: String = "DIRECTED"
  val DefaultDelimiterKey: String = "DELIMITER"
  val DefaultDelimiter: String = ","
  val DefaultCommentChar: String = "#"
  val DefaultCommentKey: String = "COMMENT"
  val DefaultEdgeHeader: Array[String] = Array("FROM", "TO", "WEIGHT")
  val DefaultEdgeColumnsKey: String = "EDGECOLUMNS"
  val DefaultEdgeColumns: String = "3"
  val DefaultNodeColumnsKey: String = "NODECOLUMNS"
  val DefaultNodeColumns: String = "2"

  val rules: Map[String, String] = getRules()
  val attributes: Map[String, String] = getAttributes()
  val Delimiter = Option(rules(DefaultDelimiterKey)).getOrElse(DefaultDelimiter)
  val Bivariate = Option(checkBooleanRule(BivariateKey)).getOrElse(false)
  val Variate = if (rules.contains(BivariateKey)) {
    Option(rules(BivariateKey)).getOrElse(0) } else { 0 }
  val Comment = Option(rules(DefaultCommentKey)).getOrElse(DefaultCommentChar)

  //* The nodes from the file, not including those inferred from edges */
  val nodesReader = if (Bivariate) {getBivariateNodes()} else {getNodes()}
  //* The edges from the file, not including those inferred from nodes */
  val edgesReader = if (Bivariate) {getBivariateEdges()} else {getEdges()}

  //* user changeable values */
  private var _directed: Boolean = checkBooleanRule(DirectedKey)
  private var _weighted: Boolean = false

  def directed: Boolean = _directed
  def weighted: Boolean = _weighted

  def directed_= (value: Boolean):Unit = _directed = value
  def weighted_= (value: Boolean):Unit = _weighted = value

  private def checkBooleanRule(value: String) = {
    if (rules.contains(value)) {
      Option(rules(value)) match {
        case Some(x: String) => x.toLowerCase match {
          case "1" | "true" | "y" | "yes" => true
          case y if (x == "BIVARIATE" && y.toInt > 1) => false
          case _ => false
        }
        case None => false
      }
    } else { false }
  }

  /** Get the maximum id value from nodes **/
  def maxNodeId(vector: Vector[Node]): String = {
     vector.map(_.nid.toLong).max.toString
  }

  def maxEdgeId(vector: Vector[Edge]): String = {
    vector.map(n => max(n.eto.toLong, n.efrom.toLong))
      .max.toString
  }

  //* Gets the rules for the configuration from the source
  private def getRules(): Map[String, String] = {
    source.filter( line => line.headOption == Some(Command))
      .flatMap( line => {
        Option(line.slice(1, line.length).split(SplitChar)) match {
          case Some(Array(a, b)) => Some(a -> b)
          case _ => None
      }
    }).toMap
  }

  private def getAttributes(): Map[String, String] = {
    val graphAttributes = source.dropWhile(x => x != GraphStart)
      .takeWhile(x => x != NodeStart)
    if (graphAttributes.hasNext) graphAttributes.next()
    val atts = graphAttributes.map(removeComments)
      .map(x => x.split(Option(rules(DefaultDelimiterKey))
          .getOrElse(DefaultDelimiter)).map(_.trim))
    val hd = if (atts.hasNext) { atts.next()
      .map(_.toUpperCase)} else { Array[String]() }
    if (atts.map(x => hd.zip(x).toMap).hasNext) {
      atts.map(x => hd.zip(x).toMap).next()
    } else { Map[String, String]()}
  }

  private def getSource(path: String) = {
    Source.fromFile(path).getLines
  }

  def getEdges(): Vector[Edge] = {
    val edgeSet = source.dropWhile(x => x != EdgeStart)
    edgeSet.next()
    val edges = edgeSet.map(removeComments)
    val hd: List[String] = edges.next match {
      case "" => (DefaultEdgeHeader ++
        (1 to rules(DefaultEdgeColumnsKey).toInt - 3)
          .map(x => x.toString)).toList
      case x => x.split(Option(rules(DefaultDelimiterKey))
        .getOrElse(DefaultDelimiter))
        .map(_.trim.toUpperCase).toList
    }
    val tail = edges
    tail.map(nestedEdges).flatMap(x => x)
      .map(x => hd.zip(x).toMap)
      .map(atts => Edge(-1, -1, atts)).toVector
  }

  def collectNodesFromIterator(itt: Iterator[String]) = {
    val regex = ("(.+) ([\\[\\(].+?[\\]\\)]" + Delimiter + ") (.+)").r
    itt.map(removeComments)
      .flatMap( x => x match {
        case "" => None
        case regex(begin, parenth, end) => Some(
          begin.split(Delimiter).map(_.trim) ++
          Array(parenth.toString.substring(0, parenth.toString.length -1)
          .replaceAll("[\\(\\)\\[\\]\\{\\}]", "").trim) ++
          end.split(Delimiter).map(_.trim))
        case _ => Some(x.split(Delimiter).map(_.trim))
      }
    )
  }

  /** Gets the nodes from the source file. **/
  def getNodes(): Vector[Node] = {
    val nodeSet = source.dropWhile(x => x != ">NODES")
      .takeWhile(x => x != ">EDGES")
    nodeSet.next()
    val nodes = collectNodesFromIterator(nodeSet)
    val hd: Array[String] = nodes.take(1).toList.head.map(_.trim.toUpperCase)
    val tail = nodes
    tail.map((x: Array[String]) => hd.zip(x).toMap)
      .toVector
      .zipWithIndex
      .map({case (atts, id) => Node(id, atts("LABEL"), atts)})
  }

  def getBivariateNodes(): Vector[Node] = {
    val regex = ("(.+) ([\\[\\(].+?[\\]\\)]" + Delimiter + ") (.+)").r
    val nodeSet = source.dropWhile(x => x != ">NODES")
    nodeSet.next()
    val nodeSetA = nodeSet.takeWhile(x => x != ">NODES")
    val nodeSetB = nodeSet.takeWhile(x => x != ">EDGES")
    val nodesA = collectNodesFromIterator(nodeSetA)
    val hdA: Array[String] = nodesA.take(1).toList.head
      .map(_.trim.toUpperCase)
    val colNodes = nodesA
      .map((x: Array[String]) => hdA.zip(x).toMap)
      .toVector
      .zipWithIndex
      .map({case (atts, id) => Node(id, atts("LABEL"),
        atts + ("NODESET" -> "COL"))})
    val nodesB = collectNodesFromIterator(nodeSetB)
    val hdB: Array[String] = nodesB.take(1).toList.head
      .map(_.trim.toUpperCase)
    val rowNodes = nodesB
      .map((x: Array[String]) => hdB.zip(x).toMap)
      .toVector
      .zipWithIndex
      .map({case (atts, id) => Node(id, atts("LABEL"),
        atts + ("NODESET" -> "ROW"))})
    colNodes ++ rowNodes
  }

  def getBivariateEdges(): Vector[Edge] = {
    Vector[Edge]()
  }

  //*
  def edgesFromEdgeList(nodeto: String, nodefrom: String): List[(String, String)] = {
    val newEdges = stringToArray(nodeto) match {
      case None => stringToArray(nodefrom) match {
        case None => List[(String, String)]()
        case Some(nodeFromList) => edgeList(nodeFromList)
      }
      case Some(nodeToList) => stringToArray(nodefrom) match {
        case None => edgeList(nodeToList)
        case Some(doubleList) => if (nodeToList.length == 1) {
          doubleList.map(n => (nodeToList(0), n))
        } else if (doubleList.length == 1) {
          nodeToList.map(n => (n, doubleList(0)))
        } else {
          for {
            ls1 <- nodeToList
            ls2 <- doubleList
          } yield (ls1, ls2)
        }
      }
    }
    newEdges
  }

  /** Creates an array from a list-like objects in str.
    * If the array contains ">ALL" notation, return None **/

  def stringToArray(str: String): Option[List[String]] = {
    str match {
      case ">ALL" => None
      case "" => None
      case x => {
        Option(x.replaceAll("[\\(\\)\\[\\]\\{\\}]", "")
          .split(Option(rules(DefaultDelimiterKey))
          .getOrElse(",")).map(_.trim).toList)
        }
    }
  }

  //* Detects tuple-like objects from str and splits them into Lists **/
  def nestedEdges(str: String): List[Vector[String]] = {
    // matches "(1, 2, 3), (4, 5, 6), 7, 8, 9"
    val regex1 = ("([\\[\\(].+?[\\]\\)]" + Delimiter +
      ") ([\\[\\(].+?[\\]\\)]" + Delimiter + ") (.+)").r
    // matches "(1, 2, 3), 4, 5, 6, 7, 8, 9"
    val regex2 = ("([\\[\\(].+?[\\]\\)]" + Delimiter + ") (>?[0-9a-zA-Z]+" +
      Delimiter + ") (.+)").r
    // matches "1, (2, 3, 4), 5, 6, 7, 8, 9"
    val regex3 = ("(>?[0-9a-zA-Z]+" + Delimiter + ") ([\\[\\(].+?[\\]\\)]" +
      Delimiter + ") (.+)").r
    str match {
      case regex1(nodefrom, nodeto, rest) => {
        edgesFromEdgeList(nodefrom, nodeto).map({ case(x, y) =>
          (Array(x, y) ++ rest.split(Delimiter)).map(_.trim).toVector
        }) }
      case regex2(nodefrom, nodeto, rest) => {
        edgesFromEdgeList(nodefrom, nodeto).map({ case(x, y) =>
          (Array(x, y) ++ rest.split(Delimiter)).map(_.trim).toVector
        }) }
      case regex3(nodefrom, nodeto, rest) => {
        edgesFromEdgeList(nodefrom, nodeto).map({ case(x, y) =>
          (Array(x, y) ++ rest.split(Delimiter)).map(_.trim).toVector
        }) }
      case _ => {
        List(str.split(Delimiter).map(_.trim).toVector)
      }
    }
  }

  def edgeList(lst: List[String]): List[(String, String)] = {
    if (directed == false) {
      lst match {
        case Nil => Nil
        case hd :: Nil => Nil
        case hd :: tail => tail.map(x => (hd, x)) ++ edgeList(tail)
      }
    } else {
      lst match {
        case Nil => Nil
        case hd :: Nil => Nil
        case hd :: tail => tail.map(x => (hd, x)) ++ tail.map(x => (x, hd)) ++
          edgeList(tail)
      }
    }
  }

  /** Gets the node id corresponding to a String **/
  def getId(ident: String, alt: Option[String] = None): Option[Long] = {
    if (nodesReader.map(x => x.attributes("ID")).contains(ident)) {
      Some(nodesReader.filter(x => x.attributes("ID") == ident)
        .map(x => x.nid).head)
    } else if (nodesReader.map(x => x.attributes("LABEL")).contains(ident)) {
      Some(nodesReader.filter(x => x.attributes("LABEL") == ident)
        .map(x => x.nid).head)
    } else {
      alt match {
        case Some(att) => Some(nodesReader.filter(x => {
          stringToArray(x.attributes(att)).getOrElse(List[String]())
          .contains(ident) })
          .map(x => x.nid).head)
        case _ => None
      }
    }
  }

  /** Removes comments based on rules. **/
  private def removeComments(line: String): String = {
    if (line.contains(Option(rules(DefaultCommentKey))
      .getOrElse(DefaultCommentChar))) {
        line.slice(0,
          line.indexOf(Option(rules(DefaultCommentKey))
          .getOrElse(DefaultCommentChar))).trim }
    else { line.trim }
  }
}
