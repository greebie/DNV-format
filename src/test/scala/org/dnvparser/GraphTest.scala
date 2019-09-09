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
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GraphTest extends FunSuite with BeforeAndAfter {
  val file = getClass.getResource("/sample_edge_list.dnv").getPath
  val file2 = getClass.getResource("/sample_edge_list2.dnv").getPath
  val fileEmpty = getClass.getResource("/sample_file_empty.dnv").getPath
  val graph = Graph(file)
  val graph2 = Graph(file2)
  val graph3 = Graph(fileEmpty)

  before {
    graph.directed = false
  }

  test("Get rules for parsing from file") {
    val rules = graph.rules
    assert(rules("COMMENT") == "#")
    assert(rules("DELIMITER") == ",")
    assert(rules("NODECOLUMNS") == "4")
    assert(rules("EDGECOLUMNS") == "4")
  }

  test("Remove Comments") {
    val testStr = graph.removeComments("Data here and # this is a comment")
    val testStr2 = graph.removeComments("## comment at start of string")
    val expected = "Data here and"
    assert(testStr == expected)
    assert(testStr2 == "")
  }

  test("Get nodes") {
    val nodes = graph.getNodes().take(3).map(x => x.attributes).toList
      .map(x => x("ID"))
    val expected = List("1", "2", "3")
    assert(nodes == expected)
  }

  test("String to Array") {
    val none = graph.stringToArray(">ALL")
    val some = graph.stringToArray("(1, 2, 3)")
    assert(none == None)
    assert(some == Some(List("1", "2", "3")))
  }

  test("Edge List") {
    val graph1 = graph.edgeList(List("A", "B", "C"))
    val expected = List(("A", "B"), ("A", "C"), ("B", "C"))
    val expected2 = List(("A", "B"), ("A", "C"), ("B", "A"), ("C", "A"),
      ("B", "C"), ("C", "B"))
    assert(graph1 == expected)
    assert(graph.edgeList(Nil) == Nil)
    graph.directed = true
    val graph2 = graph.edgeList(List("A", "B", "C"))
    assert(graph2 == expected2)
    assert(graph.edgeList(Nil) == Nil)
  }

  test("Test Edges from EdgeList") {
    val edges1 = graph.edgesFromEdgeList("""(A,B,C)""", """(A,B,C)""")
    val edges2 = graph.edgesFromEdgeList(">ALL", """(A,B,C)""")
    val edges3 = graph.edgesFromEdgeList("""(A,B,C)""", ">ALL")
    val edges4 = graph.edgesFromEdgeList(">ALL", ">ALL")
    assert(edges1(0) == ("A","A"))
    assert(edges2 == List(("A", "B"), ("A", "C"), ("B", "C")))
    assert(edges3 == List(("A", "B"), ("A", "C"), ("B", "C")))
    assert(edges4 == List[(String, String)]())
  }

  test ("Nested edges") {
    val str = """(a, b, c, d), en, f, g, h"""
    val nested = graph.nestedEdges(str)
    assert(nested == List(List("a", "en", "f", "g", "h"),
      List("b", "en", "f", "g", "h"),
      List("c", "en", "f", "g", "h"),
      List("d", "en", "f", "g", "h")))
  }

  test ("Get id") {
    val str = graph.getId("Bob Bobblewot")
    assert(str == "1")
  }

  test("Get Attributes") {
    val expected = Map( "name" -> "Example Graph",
      "description" -> "An Example Graph")
    assert(graph.attributes == expected)
  }


  test("Get edges") {
    val edges = graph.edges.take(3).map(x => x.attributes)
    val edges2 = graph2.edges.take(3).map(x => x.attributes)
    assert(edges.map(x => x("TO")) == Vector("1", "1", "1"))
    assert(edges.map(x => x("FROM")) == List("2", "3", "1"))
    assert(edges.map(x => x("WEIGHT")) == List("1", "1", "1"))
    assert(edges.map(x => x("SENTIMENT")) == List("0.123", "-0.5", "0.999"))
    assert(edges2(0) == Map("TO" -> "1", "FROM" -> "2", "WEIGHT" -> "1"))
  }
}
