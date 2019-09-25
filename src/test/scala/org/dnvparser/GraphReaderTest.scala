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
class GraphReaderTest extends FunSuite with BeforeAndAfter {
  val file = getClass.getResource("/sample_edge_list.dnv").getPath
  val file2 = getClass.getResource("/sample_edge_list2.dnv").getPath
  println(file)
  val graphReader = new GraphReader {
    def path = file
  }
  val graphReader2 = new GraphReader {
    def path = file2
  }

  test("Get rules for parsing from file") {
    val rules = graphReader.rules
    assert(rules("COMMENT") == "#")
    assert(rules("DELIMITER") == ",")
    assert(rules("NODECOLUMNS") == "4")
    assert(rules("EDGECOLUMNS") == "4")
  }

  test("Get Attributes") {
    val expected = Map( "NAME" -> "Example Graph",
      "DESCRIPTION" -> "An Example Graph")
    assertResult(expected) { graphReader.attributes }
    assertResult(Map[String, String]()){graphReader2.attributes}
  }

  test("Set variables") {
    assert(graphReader.directed)
    assert(!graphReader.weighted)
    graphReader.directed = false
    graphReader.weighted = true
    assert(!graphReader.directed)
    assert(graphReader.weighted)
  }
  test("String to Array") {
    val none = graphReader.stringToArray(">ALL")
    val some = graphReader.stringToArray("(1, 2, 3)")
    assert(none == None)
    assert(some == Some(List("1", "2", "3")))
  }

  test("Edge List") {
    val graph1 = graphReader.edgeList(List("A", "B", "C"))
    val expected = List(("A", "B"), ("A", "C"), ("B", "C"))
    val expected2 = List(("A", "B"), ("A", "C"), ("B", "A"), ("C", "A"),
      ("B", "C"), ("C", "B"))
    assert(graph1 == expected)
    assert(graphReader.edgeList(Nil) == Nil)
    graphReader.directed = true
    val graph2 = graphReader.edgeList(List("A", "B", "C"))
    assert(graph2 == expected2)
    assert(graphReader.edgeList(Nil) == Nil)
  }

  test("Test Edges from EdgeList") {
    graphReader.directed = false
    val edges1 = graphReader.edgesFromEdgeList("""(A,B,C)""", """(A,B,C)""")
    val edges2 = graphReader.edgesFromEdgeList(">ALL", """(A,B,C)""")
    val edges3 = graphReader.edgesFromEdgeList("""(A,B,C)""", ">ALL")
    val edges4 = graphReader.edgesFromEdgeList(">ALL", ">ALL")
    assert(edges1(0) == ("A","A"))
    assert(edges2 == List(("A", "B"), ("A", "C"), ("B", "C")))
    assert(edges3 == List(("A", "B"), ("A", "C"), ("B", "C")))
    assert(edges4 == List[(String, String)]())
  }

  test ("Get id") {
    val id = graphReader.getId("Bob Bobblewot")
    val id2 = graphReader.getId("Robert", Some("PSEUDO"))
    val id3 = graphReader.getId("George")
    val id4 = graphReader.getId("George")
    val id5 = graphReader.getId("6")
    assert(id == Some(0))
    assert(id2 == Some(0))
    assert(id3 == None)
    assert(id5 == Some(4))
    assert(id4 == None)

  }
}
