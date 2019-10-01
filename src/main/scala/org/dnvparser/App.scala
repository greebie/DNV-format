
package org.dnvparser

import java.io.File
import java.io.PrintWriter
import scala.io.Source

/**
 * @author ryan.deschamps
 */
object App {
  def main(args: Array[String]): Unit = {
    val bipath = "/Users/ryandeschamps/cybersecurity.dnv"
    val path = getClass.getResource("").getPath
    val inpath = getClass.getResource("/example.dnv").getPath
    val writer = if (args.length < 2) {
      new PrintWriter(new File(path + "/output"))} else {
        Option(args(1)) match {
      case Some(x) => new PrintWriter(new File(x))
      case None => new PrintWriter(new File(path + "/output"))}
    }
    val file = if (args.isEmpty) {inpath} else {Option(args(0)) match {
      case Some(x) => x
      case None => inpath
    }}
    val graph = Network(file)
    val bigraph = Bivariate(bipath)
    // bigraph.plotCoords()
    // bigraph.removeSingle = false
    val eigDegree = graph.eigenVectorCentrality()
    val eigNormal = graph.normalizeValues(eigDegree).toArray
    val mat = graph.nodeSet().toArray.zip(eigNormal)
    mat.foreach(x => writer.write(x.toString + "\n"))
    writer.close()
  }
}
