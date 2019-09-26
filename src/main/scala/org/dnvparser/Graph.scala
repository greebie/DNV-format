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
import scala.collection
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory
import scala.math.max


trait Graph extends GraphReader {
  case class NodesState(maxNodeId: String, nodes: Vector[(Node, Int)])
  val nodes: Vector[Node] = getAllNodes()
  val edges: Vector[Edge] = getAllEdges()

  def getAllNodes(): Vector[Node] = {
    val nodes: Vector[Node] = nodesReader
    val hd: Iterable[String] = nodesReader.map(_.attributes.keySet).head
    val start = maxNodeId(nodes).toLong + 1
    val edges = edgesReader
      .filter(edge => getId(edge.attributes("TO")) == None ||
        getId(edge.attributes("FROM")) == None)
      .flatMap(edge => Vector(
        // if there is no node for an edge, keep the edge name for now.
        // then flatten them into a list of nodes with no Node object.
        getId(edge.attributes("TO")) match {
          case None => edge.attributes("TO")
          case Some(x) => -1 },
        getId(edge.attributes("FROM")) match {
          case None => edge.attributes("FROM")
          case Some(x) => -1
        }))
      .filter(node => node != -1)
      .distinct
      .zipWithIndex.map({ case (n, ind) => (n, ind + start) })
      .map(n => Node(n._2, n._1.toString,
        hd.map(x => x match {
          case "ID" => (x, n._1.toString)
          case "LABEL" => (x, n._1.toString)
          case _ => (x, "")
        }).toMap)).toVector
    (nodes ++ edges).distinct
  }

  def getAllEdges(): Vector[Edge] = {
    edgesReader.map(x => {
      val fromId: Long = getIdAll(x.attributes("FROM")).getOrElse(-1)
      val toId: Long = getIdAll(x.attributes("TO")).getOrElse(-1)
      Edge(fromId, toId,
        Map("FROM" -> getNodeById(fromId).getOrElse(Node(-1, "", Map())).label,
          "TO" -> getNodeById(toId).getOrElse(Node(-1, "", Map())).label) ++
          x.attributes.drop(2))})
      .filter(x => x.eto != -1 || x.efrom != -1 || x.attributes("FROM") != "" ||
        x.attributes("TO") != "")
  }

  /** Gets the node based on the id **/
  def getNodeByIdentifier(nodeId: String): Option[Node] = {
    getIdAll(nodeId) match {
      case Some(nid) => Some(nodes.filter(x => x.nid == nid).head)
      case None => None
    }
  }

  def getNodeById(nodeId: Long) : Option[Node] = {
    nodes.filter(x => x.nid == nodeId).headOption
  }

  /** Gets the node id corresponding to a String **/
  def getIdAll(ident: String, alt: Option[String] = None): Option[Long] = {
    val nodex = nodes
    val delimiter = Option(rules(DefaultDelimiterKey))
      .getOrElse(DefaultDelimiter)
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

  // End of Class
}

class GraphImpl(val path: String) extends Graph {

}

object Graph {
  def apply(path: String) = {
    new GraphImpl(path)
  }
}
