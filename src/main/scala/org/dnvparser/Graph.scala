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

package org.igraph

import scala.io.Source
import scala.collection
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

trait Graph {
  val logger = Logger(LoggerFactory.getLogger("IGraph Graph Functions"))
  def attributes: Map[String, String] = Map[String, String]()
  def directed: Boolean = false
  def nodes: Vector[Node] = getNodes()
  def edges: Vector[Edge] = Vector[Edge]()
  def path: String
  val rules = getRules()

  def removeComments(line: String) = {
    if (line.contains(Option(rules("COMMENT")).getOrElse("#"))) {
      line.slice(0,
        line.indexOf(Option(rules("COMMENT")).getOrElse("#"))).trim }
    else { line.trim }
  }

  def getRules() = {
    val source: Iterator[String] = Source.fromFile(path).getLines
    source
      .filter( line => line.headOption == Some('>'))
      .flatMap( line => {
        Option(line.slice(1, line.length).split("=")) match {
          case Some(Array(a, b)) => Some(a -> b)
          case None => None
          case _ => None
        }
      }).toMap
    }

  def getNodes() = {
    val source = Source.fromFile(path).getLines
    val nodeSet = source.dropWhile(x => x != ">NODES")
      .takeWhile(x => x != ">EDGES")
    nodeSet.next()
    val nodes = nodeSet.map(removeComments)
      .flatMap( x => x match {
        case "" => None
        case x => Some(x.split(Option(rules("DELIMITER")).getOrElse(",")))
      })
    val hd: Array[String] = nodes.take(1).toList.head
    val tail = nodes
    tail.map((x: Array[String]) => Node(hd.zip(x).toMap)).toVector
  }

  def getEdges() = {
    val source = Source.fromFile(path).getLines
    val edgeSet = source.dropWhile(x => x != ">EDGES")
    edgeSet.next()
    val edges = edgeSet.map( x => {
      if (x.contains(Option(rules("COMMENT")).getOrElse("#"))) {
        x.slice(0,
          source.indexOf(Option(rules("COMMENT")).getOrElse("#"))).trim }
      else { x.trim }
    })
  }
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
