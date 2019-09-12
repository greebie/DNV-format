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
class NetworkTest extends FunSuite with BeforeAndAfter {
  val file = getClass.getResource("/sample_edge_list.dnv").getPath
  val file2 = getClass.getResource("/sample_edge_list2.dnv").getPath
  val network = Network(file)

  test ("Get neighbours of Node") {
    val nodeIdent: String = "Bob Bobblewot"
    val nodeIdent2: String = "George"
    assert(network.outNeighbors(nodeIdent).map(_.nid) == Vector(1, 2, 0))
    assert(network.inNeighbors(nodeIdent).map(_.nid) == Vector(0, 1, 2, 3))
    assert(network.neighbors(nodeIdent).map(_.nid) == Vector(1, 2, 0, 3))
    assert(network.outNeighbors(nodeIdent2).map(_.nid) == Vector(4, 6, 7))
    assert(network.inNeighbors(nodeIdent2).map(_.nid) == Vector())
    assert(network.neighbors(nodeIdent2).map(_.nid) == Vector(4, 6, 7))
  }

}
