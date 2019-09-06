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
import org.scalatest.Matchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GraphTest extends FunSuite with Matchers {
  val file = getClass.getResource("/sample_edge_list.dnv").getPath

  test("Get rules for parsing from file") {
    val rules = Graph(file).rules
    assert(rules("COMMENT") == "#")
    assert(rules("DELIMITER") == ",")
    assert(rules("NODECOLUMNS") == "4")
    assert(rules("EDGECOLUMNS") == "5")
  }

  test("Get nodes") {
    val nodes = Graph(file).getNodes().take(3).map(x => x.attributes).toList
      .map(x => x("ID"))
    val expected = List("1", "2", "3")
    assert(nodes == expected)
  }

  test("Edge List") {
    val graph = Graph(file)
    val graph1 = graph.edgeList(List("A", "B", "C"))
    val expected = List(("A", "B"), ("A", "C"), ("B", "C"))
    val expected2 = List(("A", "B"), ("A", "C"), ("B", "A"), ("C", "A"),
      ("B", "C"), ("C", "B"))
    assert(graph1 == expected)
    graph.directed = true
    val graph2 = graph.edgeList(List("A", "B", "C"))
    assert(graph2 == expected2)
  }

  test("Test Edges from EdgeList") {
    val graph = Graph(file)
    val edges1 = graph.edgesFromEdgeList("""(A,B,C)""", """(A,B,C)""")
    val edges2 = graph.edgesFromEdgeList(">ALL", """(A,B,C)""")
    assert(edges1(0) == ("A","A"))
    assert(edges2 == List(("A", "B"), ("A", "C"), ("B", "C")))
  }

  test ("Nested edges") {
    val str = """(a, b, c, d), en, f, g, h"""
    val nested = Graph(file).nestedEdges(str)
    assert(nested == List(List("a", "en", "f", " g", " h"),
      List("b", "en", "f", " g", " h"),
      List("c", "en", "f", " g", " h"),
      List("d", "en", "f", " g", " h")))
  }

  test("Get edges") {
  }
}
