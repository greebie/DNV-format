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
import org.apache.commons.io.FileUtils
import org.scalatest.{AsyncFunSuite, BeforeAndAfter}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg.{DenseMatrix}
import scala.util.Try
import java.io.File
import java.nio.file.{Files, Paths}


@RunWith(classOf[JUnitRunner])
class AppTest extends AsyncFunSuite with BeforeAndAfter {
  val file = getClass.getResource("/sample_edge_list.dnv").getPath
  val output = getClass.getResource("").getPath + "/output"
  val start = App.main(Array(file, output))

  before {

  }

  after {
    if (Files.exists(Paths.get(output))) {
         Try (FileUtils.deleteDirectory(new File(output)))
       }
  }

  test("Add file name to main") {
    assert(start.getClass.toString() == "void")
  }

  test("Check output file details") {
    val test = Source.fromFile(output).getLines.take(5).toVector
    val expected = Vector(
      "((0,Bob Bobblewot),0.0)",
      "((1,Sandy Poland),0.24124095066299805)",
      "((2,Anderson Li),0.4749997675376122)",
      "((3,4),0.38224854206815667)",
      "((4,8),0.11675351812220724)"
    )
    assert(test == expected)
  }




}
