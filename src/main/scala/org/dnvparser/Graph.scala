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


trait Graph {
  val logger = Logger(LoggerFactory.getLogger("DNVParser Graph Functions"))
  def attributes: Map[String, String] = getAttributes()
  var directed: Boolean = false
  def nodes: Vector[Node] = getNodes()
  def edges: Vector[Edge] = getEdges()
  def path: String // path to file
  val rules: Map[String, String] = getRules() // config options

  /** Removes comments based on rules. **/
  def removeComments(line: String): String = {
    if (line.contains(Option(rules("COMMENT")).getOrElse("#"))) {
      line.slice(0,
        line.indexOf(Option(rules("COMMENT")).getOrElse("#"))).trim }
    else { line.trim }
  }

  def getId(id: String): String = {
    if (nodes.map(x => x.attributes("ID")).contains(id)) {
      id
    } else if (nodes.map(x => x.attributes("LABEL")).contains(id)) {
      nodes.filter(x => x.attributes("LABEL") == id)
        .map(x => x.attributes("ID")).head
    } else {
      "ADD" + (nodes.length + 1).toString
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
    val source = Source.fromFile(path).getLines
    val nodeSet = source.dropWhile(x => x != ">NODES")
      .takeWhile(x => x != ">EDGES")
    nodeSet.next()
    val nodes = nodeSet.map(removeComments)
      .flatMap( x => x match {
        case "" => None
        case x => Some(x.split(Option(rules("DELIMITER"))
          .getOrElse(","))
          .map(_.trim))
      })
    val hd: Array[String] = nodes.take(1).toList.head
    val tail = nodes
    tail.map((x: Array[String]) => Node(hd.zip(x).toMap)).toVector
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
      .map(x => x.patch(0, Seq(getId(x(0)), getId(x(1))), 2))
      .map(x => Edge(hd.zip(x).toMap)).toVector
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

trait Node {
  def attributes: Map[String, String]
}

class NodeImpl (val attributes: Map[String, String]) extends Node {

}

object Node {
  def apply(attr: Map[String, String]) = {
    new NodeImpl(attr)
  }
}

trait Edge {
  def attributes: Map[String, String]
}

class EdgeImpl (val attributes: Map[String, String]) extends Edge {

}

object Edge {
  def apply(attr: Map[String, String]) = {
    new EdgeImpl(attr)
  }
}
