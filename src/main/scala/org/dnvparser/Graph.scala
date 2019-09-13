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
  val logger = Logger(LoggerFactory.getLogger("DNVParser Graph Functions"))
  val rules: Map[String, String] = getRules() // config options
  def attributes: Map[String, String] = getAttributes()
  var directed: Boolean = false
  val firstNodes: Vector[Node] = getNodes()
  val firstEdges: Vector[Edge] = getEdges()
  val nodes: Vector[Node] = getAllNodes()
  val edges: Vector[Edge] = getAllEdges()
  def path: String // path to file

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
    val nodes: Vector[Node] = firstNodes
    val hd: Iterable[String] = firstNodes.map(_.attributes.keySet).head
    val start = maxNodeId(nodes).toLong + 1
    nodes ++ firstEdges
      .filter(edge => getId(edge.attributes("TO")) == None ||
        getId(edge.attributes("FROM")) == None)
      .flatMap(edge => List(
        // if there is no node for an edge, keep the edge name for now.
        // then flatten them into a list of nodes with no Node object.
        getId(edge.attributes("TO")) match {
          case None => edge.attributes("TO")
          case Some(x) => -1 },
        getId(edge.efrom.toString) match {
          case None => edge.attributes("FROM")
          case Some(x) => -1
        }))
      .filter(node => node != -1)
      .distinct
      // auto number the nodes to create ids.
      .zipWithIndex.map({ case (n, ind) => (n, ind + start) })
      .map(n => Node(n._2, n._1.toString,
        hd.map(x => x match {
          case "ID" => (x, n._1.toString)
          case "LABEL" => (x, n._1.toString)
          case _ => (x, "")
        }).toMap)).toVector
  }

  def getAllEdges(): Vector[Edge] = {
    firstEdges.map(x => Edge(getId(x.attributes("FROM"), all_nodes=true).getOrElse(-1),
        getId(x.attributes("TO"), all_nodes=true).getOrElse(-1), x.attributes))
        .filter(x => x.eto != -1 || x.efrom != -1)
  }

  /** Gets the node based on the id **/
  def getNodeByIdentifier(nodeId: String): Option[Node] = {
    getId(nodeId) match {
      case Some(nid) => Some(nodes.filter(x => x.nid == nid).head)
      case None => None
    }
  }

  def getNodeById(nodeId: Long) : Option[Node] = {
    nodes.filter(x => x.nid == nodeId).headOption
  }

  /** Gets the node id corresponding to a String **/
  def getId(ident: String, alt: Option[String] = None,
    all_nodes: Boolean = false): Option[Long] = {
    val nodex = if (all_nodes) nodes  else firstNodes
    val delimiter = Option(rules("DELIMITER")).getOrElse(",")
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
      case regex1(nodefrom, nodeto, rest) => {
        edgesFromEdgeList(nodefrom, nodeto).map({ case(x, y) =>
          (Array(x, y) ++ rest.split(delimiter)).map(_.trim).toList
        }) }
      case regex2(nodefrom, nodeto, rest) => {
        edgesFromEdgeList(nodefrom, nodeto).map({ case(x, y) =>
          (Array(x, y) ++ rest.split(delimiter)).map(_.trim).toList
        }) }
      case regex3(nodefrom, nodeto, rest) => {
        edgesFromEdgeList(nodefrom, nodeto).map({ case(x, y) =>
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
    val hd: Array[String] = nodes.take(1).toList.head.map(_.trim.toUpperCase)
    val tail = nodes
    tail.map((x: Array[String]) => hd.zip(x).toMap)
      .toVector
      .zipWithIndex
      .map({case (atts, id) => Node(id, atts("LABEL"), atts)})
  }

  def getEdges(): Vector[Edge] = {
    val source = Source.fromFile(path).getLines
    val edgeSet = source.dropWhile(x => x != ">EDGES")
    edgeSet.next()
    val edges = edgeSet.map(removeComments)
    val hd: List[String] = edges.next match {
      case "" => (Array("FROM", "TO", "WEIGHT") ++
        (1 to rules("EDGECOLUMNS").toInt - 3)
          .map(x => x.toString)).toList
      case x => x.split(Option(rules("DELIMITER")).getOrElse(","))
        .map(_.trim.toUpperCase).toList
    }
    val tail = edges
    tail.map(nestedEdges).flatMap(x => x)
      .map(x => hd.zip(x).toMap)
      .map(atts => Edge(-1, -1, atts)).toVector
  }

  def getAttributes(): Map[String, String] = {
    val source = Source.fromFile(path).getLines
    val graphAttributes = source.dropWhile(x => x != ">GRAPH")
      .takeWhile(x => x != ">NODES")
    if (graphAttributes.hasNext) graphAttributes.next()
    val atts = graphAttributes.map(removeComments)
      .map(x => x.split(Option(rules("DELIMITER"))
          .getOrElse(",")).map(_.trim))
    val hd = if (atts.hasNext) { atts.next()
      .map(_.toUpperCase)} else { Array[String]() }
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
