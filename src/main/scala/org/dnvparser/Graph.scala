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

import scala.io.Source
import scala.collection
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import scala.math.max


trait Graph {
  case class NodesState(maxNodeId: String, nodes: Vector[(Node, Int)])
  case class Node(nid: String, label: String, attributes: Map[String, String])
  case class Edge(eto: String, efrom: String, attributes: Map[String, String])
  val logger = Logger(LoggerFactory.getLogger("DNVParser Graph Functions"))
  def attributes: Map[String, String] = getAttributes()
  var directed: Boolean = false
  def edges: Vector[Edge] = getEdges()
  def nodes: Vector[Node] = getAllNodes()
  def path: String // path to file
  val rules: Map[String, String] = getRules() // config options

  /** Removes comments based on rules. **/
  def removeComments(line: String): String = {
    if (line.contains(Option(rules("COMMENT")).getOrElse("#"))) {
      line.slice(0,
        line.indexOf(Option(rules("COMMENT")).getOrElse("#"))).trim }
    else { line.trim }
  }

  /** Get the maximum id value from nodes **/
  def maxNodeId(vector: Vector[Node]): String = {
     vector.map(_.nid.toLong).max.toString
  }

  def maxEdgeId(vector: Vector[Edge]): String = {
    vector.map(n => max(n.eto.toLong, n.efrom.toLong))
      .max.toString
  }

  def getAllNodes(): Vector[Node] = {
    val nodes: Vector[Node] = getNodes()
    val hd: Iterable[String] = getNodes().map(_.attributes.keys).head
    val start = maxNodeId(nodes).toLong + 1
    nodes ++ getEdges()
      .filter(edge => getId(edge.eto) == None || getId(edge.efrom) == None)
      .flatMap(edge => List(
        getId(edge.eto) match {
          case None => edge.eto
          case _ => "" },
        getId(edge.efrom) match {
          case None => edge.efrom
          case _ => ""
        }))
      .filter(node => node != "")
      .distinct
      .zipWithIndex
      .map(n => Node((start + n._2).toString,
        n._1.toString, hd.map(x => x match {
          case "ID" => (x, n._1)
          case "LABEL" => (x, n._1)
          case _ => (x, "")
        }).toMap)).toVector
  }

  /** Adds a node to nodes **/
  def addNode(node: Map[String, String]): Vector[Node] = {
    nodes :+ Node((maxNodeId(nodes) + 1).toString, node("LABEL"), node)
  }

  /** Gets the node id corresponding to a String **/
  def getId(id: String, alt: Option[String] = None): Option[String] = {
    val nodes = getNodes()
    val delimiter = Option(rules("DELIMITER")).getOrElse(",")
    if (nodes.map(x => x.nid).contains(id)) {
      Some(id)
    } else if (nodes.map(x => x.attributes("ID")).contains(id)) {
      Some(nodes.filter(x => x.attributes("ID") == id)
        .map(x => x.nid).head.toString)
    } else if (nodes.map(x => x.attributes("LABEL")).contains(id)) {
      Some(nodes.filter(x => x.attributes("LABEL") == id)
        .map(x => x.nid).head.toString)
    } else {
      alt match {
        case Some(att) => Some(nodes.filter(x => {
          stringToArray(x.attributes(att)).getOrElse(List[String]())
          .contains(id) })
          .map(x => x.nid).head.toString)
        case _ => None
      }
    }
  }

  /** Get the set of rules from the source file. **/
  def getRules(): Map[String, String] = {
    val source: Iterator[String] = Source.fromFile(path).getLines
    source.filter( line => line.headOption == Some('>'))
      .flatMap( line => {
        Option(line.slice(1, line.length).split("=")) match {
          case Some(Array(a, b)) => Some(a -> b)
          case _ => None
      }
    }).toMap
  }

  def stringToArray(str: String): Option[List[String]] = {
    str match {
      case ">ALL" => None
      case x => {
        Some(x.replaceAll("[\\(\\)\\[\\]\\{\\}]", "")
          .split(Option(rules("DELIMITER")).getOrElse(",")).map(_.trim).toList)
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

  def nestedEdges(str: String): List[List[String]] = {
    val delimiter = Option(rules("DELIMITER")).getOrElse(",")
    // matches "(1, 2, 3), (4, 5, 6), 7, 8, 9"
    val regex1 = ("([\\[\\(].+?[\\]\\)]" + delimiter +
      ") ([\\[\\(].+?[\\]\\)]" + delimiter + ") (.+)").r
    // matches "(1, 2, 3), 4, 5, 6, 7, 8, 9"
    val regex2 = ("([\\[\\(].+?[\\]\\)]" + delimiter + ") (>?[0-9a-zA-Z]+" +
      delimiter + ") (.+)").r
    // matches "1, (2, 3, 4), 5, 6, 7, 8, 9"
    val regex3 = ("(>?[0-9a-zA-Z]+" + delimiter + ") ([\\[\\(].+?[\\]\\)]" +
      delimiter + ") (.+)").r
    str match {
      case regex1(nodeto, nodefrom, rest) => {
        edgesFromEdgeList(nodeto, nodefrom).map({ case(x, y) =>
          (Array(x, y) ++ rest.split(delimiter)).map(_.trim).toList
        }) }
      case regex2(nodeto, nodefrom, rest) => {
        edgesFromEdgeList(nodeto, nodefrom).map({ case(x, y) =>
          (Array(x, y) ++ rest.split(delimiter)).map(_.trim).toList
        }) }
      case regex3(nodeto, nodefrom, rest) => {
        edgesFromEdgeList(nodeto, nodefrom).map({ case(x, y) =>
          (Array(x, y) ++ rest.split(delimiter)).map(_.trim).toList
        }) }
      case _ => {
        List(str.split(delimiter).map(_.trim).toList)
      }
    }
  }

  /** Gets the nodes from the source file. **/
  def getNodes(): Vector[Node] = {
    val delimiter = Option(rules("DELIMITER")).getOrElse(",")
    val regex = ("(.+) ([\\[\\(].+?[\\]\\)]" + delimiter + ") (.+)").r
    val source = Source.fromFile(path).getLines
    val nodeSet = source.dropWhile(x => x != ">NODES")
      .takeWhile(x => x != ">EDGES")
    nodeSet.next()
    val nodes = nodeSet.map(removeComments)
      .flatMap( x => x match {
        case "" => None
        case regex(begin, parenth, end) => Some(begin
          .split(delimiter)
          .map(_.trim) ++ Array(parenth.toString
            .substring(0, parenth.toString.length -1)
            .replaceAll("[\\(\\)\\[\\]\\{\\}]", "")
            .trim) ++
          end.split(delimiter).map(_.trim))
        case _ => Some(x.split(Option(rules("DELIMITER"))
          .getOrElse(","))
          .map(_.trim))
      })
    val hd: Array[String] = nodes.take(1).toList.head
    val tail = nodes
    tail.map((x: Array[String]) => hd.zip(x).toMap)
      .toVector
      .zipWithIndex
      .map({case (atts, id) => Node(id.toString, atts("LABEL"), atts)})
  }

  def getEdges(): Vector[Edge] = {
    val source = Source.fromFile(path).getLines
    val edgeSet = source.dropWhile(x => x != ">EDGES")
    edgeSet.next()
    val edges = edgeSet.map(removeComments)
    val hd: List[String] = edges.next match {
      case "" => (Array("TO", "FROM", "WEIGHT") ++
        (1 to rules("EDGECOLUMNS").toInt - 3)
          .map(x => x.toString)).toList
      case x => x.split(Option(rules("DELIMITER")).getOrElse(","))
        .map(_.trim).toList
    }
    val tail = edges
    tail.map(nestedEdges).flatMap(x => x)
      .map(x => hd.zip(x).toMap)
      .map(atts => Edge(getId(atts("TO")).getOrElse(atts("TO")),
        getId(atts("FROM")).getOrElse(atts("FROM")), atts)).toVector
  }

  def getAttributes(): Map[String, String] = {
    val source = Source.fromFile(path).getLines
    val graphAttributes = source.dropWhile(x => x != ">GRAPH")
      .takeWhile(x => x != ">NODES")
    if (graphAttributes.hasNext) graphAttributes.next()
    val atts = graphAttributes.map(removeComments)
      .map(x => x.split(Option(rules("DELIMITER"))
          .getOrElse(",")).map(_.trim))
    val hd = if (atts.hasNext) atts.next() else Array[String]()
    if (atts.map(x => hd.zip(x).toMap).hasNext) {
      atts.map(x => hd.zip(x).toMap).next()
    } else { Map[String, String]()}
  }
  // End of Class
}

class GraphImpl(val path: String) extends Graph {

}

object Graph {
  def apply(path: String) = {
    new GraphImpl(path)
  }
}
