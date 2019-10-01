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
import org.scalatest.{AsyncFunSuite, BeforeAndAfter, Matchers}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg.{DenseMatrix, *}
import scala.util.Try
import java.io.File
import java.nio.file.{Files, Paths}
import scala.concurrent.Future


@RunWith(classOf[JUnitRunner])
class BivariateTest extends AsyncFunSuite with BeforeAndAfter with Matchers {
  val file = getClass.getResource("/bivariate_sample.dnv").getPath
  val bivariate = Bivariate(file)
  val tolerance = 0.001 // Floating point precision for tests

  before {
    bivariate.removeSingle = true
  }


  test ("Gets the bivariate file") {
    val test = bivariate.nodesReader
    val edgeTest = bivariate.edges
    val cols = bivariate.columns
    val rows = bivariate.rows
    val labelsCol = test.filter(x => x.attributes("NODESET") == "COL")
      .map(x => x.label)
    val labelsRow = test.filter(x => x.attributes("NODESET") == "ROW")
      .map(x => x.label)
    labelsCol should equal (Vector("Canada", "China", "India",
        "US", "Mexico"))
    labelsRow should equal (Vector("Asian", "North America", "Superpowers",
      "Rice"))
    rows.map(x => x.label) should equal (Vector("Asian", "North America",
      "Superpowers", "Rice", "7"))
    cols.map(x => x.label) should equal (Vector("Canada", "China", "India",
        "US", "Mexico", "6"))
  }

  test("Creates a n X m Matrix") {
    val test = bivariate.createMatrix()
    val colLabels = Vector("Asian", "North America", "Superpowers", "Rice", "7")
    val rowLabels = Vector("Canada", "China", "India", "US", "Mexico", "6")
    val expected: DenseMatrix[Double] = DenseMatrix(
      (0.0,1.0,1.0,0.0,0.0,0.0),
      (1.0,0.0,0.0,1.0,1.0,0.0),
      (0.0,1.0,0.0,1.0,0.0,0.0),
      (0.0,1.0,1.0,0.0,1.0,0.0),
      (0.0,0.0,0.0,0.0,0.0,1.0))
    test should be (BivariateMatrix(
      colLabels,
      colLabels,
      rowLabels,
      rowLabels,
      expected))
    bivariate.removeSingle = false
    test.matrix should equal (bivariate.matrix)
  }

  test("Removes Singles") {
    val removed = bivariate.removeSingles()
    val rowLabels = Vector("Asian", "North America", "Superpowers", "Rice")
    val colLabels = Vector("China", "India", "US", "Mexico")
    val bivMatrix = (BivariateMatrix(
      rowLabels, rowLabels,
      colLabels, colLabels,
      DenseMatrix(
        (1.0, 1.0, 0.0, 0.0),
        (0.0, 0.0, 1.0, 1.0),
        (1.0, 0.0, 1.0, 0.0),
        (1.0, 1.0, 0.0, 1.0))))
    removed should be (bivMatrix)
    bivariate.rowLabels should equal (rowLabels)
    bivariate.colLabels should equal (colLabels)
  }

  test("Correspondence Values") {

    val number = 9.0
    val residual1 = 0.0185185
    val residual2 = 0.04938
    val stand1 = -1.2546
    val stand2 = 0.8164
    val stand3 = -0.5448
    val stand4 = 0.6273
    val dim1 = 0.5378
    val dim2 = 0.375
    val profile1 = 1.0
    val profile2 = 0.0
    val avg1 = 0.0247
    val avg2 = 0.03704
    bivariate.n should equal (number)
    bivariate.ResidualsSS(0,0) should equal (residual1 +- tolerance)
    bivariate.ResidualsSS(0,3) should equal (residual2 +- tolerance)
    bivariate.standCoordsRow(0,0) should equal (stand1 +- tolerance)
    bivariate.standCoordsRow(0,3) should equal (stand2 +- tolerance)
    bivariate.standCoordsCol(0,0) should equal (stand3 +- tolerance)
    bivariate.standCoordsCol(0,3) should equal (stand4 +- tolerance)
    bivariate.rowCoords(0, 0) should equal (stand1 +- tolerance)
    bivariate.colCoords(0,0) should equal (stand3 +- tolerance)
    bivariate.dimensions(0) should equal (dim1 +- tolerance)
    bivariate.dimensions(1) should equal (dim2 +- tolerance)
    bivariate.colProfile(1, 2) should equal (profile1 +- tolerance)
    bivariate.rowProfile(1,1) should equal (profile2 +- tolerance)
    bivariate.avgColProfile(0) should equal (avg1 +- tolerance)
    bivariate.avgRowProfile(0) should equal (avg2 +- tolerance)
  }
}
