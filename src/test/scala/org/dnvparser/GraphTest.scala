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

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GraphTest extends FunSuite with BeforeAndAfter {
  val file = getClass.getResource("/sample_edge_list.dnv").getPath
  val file2 = getClass.getResource("/sample_edge_list2.dnv").getPath
  val graph = Graph(file)
  val graph2 = Graph(file)

  before {
    graph.directed = false
  }

  test("Get maximum of nodes or edges") {
    val nodes = graph.maxNodeId(graph.nodes)
    val edges = graph.maxEdgeId(graph.edges)
    assert(nodes.toInt == 8)
  }

  test("Get a Node by Id") {
    val node = graph.getNodeByIdentifier("Bob Bobblewot").getOrElse(Node())
    val node2 = graph.getNodeById(0).getOrElse(Node(7, "Not Bob",
      Map[String, String]()))
    val node3 = graph.getNodeByIdentifier("ZongoLongon")
    assert(node.nid == 0)
    assert(node == node2)
    assert(node.label == "Bob Bobblewot")
    assert(node3 == None)
  }

  test("Get nodes") {
    val nodes = graph.getNodes().take(3).map(x => x.attributes).toList
      .map(x => x("ID"))
    val expected = List("1", "2", "3")
    assert(nodes == expected)
  }


  test ("Get id") {
    val id = graph.getIdAll("Bob Bobblewot")
    val id2 = graph.getIdAll("Robert", Some("PSEUDO"))
    val id3 = graph.getIdAll("George")
    val id4 = graph.getIdAll("6")
    assert(id == Some(0))
    assert(id2 == Some(0))
    assert(id3 == Some(6))
    assert(id4 == Some(4))
  }

  test ("Nested edges") {
    val str = """(a, b, c, d), en, f, g, h"""
    val nested = graph.nestedEdges(str)
    assert(nested == List(List("a", "en", "f", "g", "h"),
      List("b", "en", "f", "g", "h"),
      List("c", "en", "f", "g", "h"),
      List("d", "en", "f", "g", "h")))
  }

  test("Get edges") {
    val edges = graph.edges.take(3).map(x => x.eto)
    val edges1 = graph.edges.take(3).map(x => x.efrom)
    val edges2 = graph.edges.take(3).map(x => x.attributes)
    val edges3 = graph2.edges.take(3).map(x => x.attributes)
    assert(edges == Vector(1, 2, 0))
    assert(edges1 == Vector(0, 0, 0))
    assert(edges2.map(x => x("WEIGHT")) == Vector("1", "1", "1"))
    assert(edges2.map(x => x("SENTIMENT")) == Vector("0.123", "-0.5", "0.999"))
    assert(edges3(0) == Map("FROM" -> "Bob Bobblewot", "TO" -> "Sandy Poland",
      "WEIGHT" -> "1", "SENTIMENT" -> "0.123"))
  }

  test("Get All Nodes") {
    val nodes = graph.nodes.toList
    assert(nodes.map(x => x.nid) == List(0, 1, 2, 3, 4, 5, 6, 7, 8))
    assert(nodes.map(x => x.label) == List("Bob Bobblewot",
      "Sandy Poland", "Anderson Li", "4", "6", "8", "George", "9", "10"))
  }
}
