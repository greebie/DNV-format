
package org.dmvparser

import java.io.File
import java.io.PrintWriter
import scala.io.Source

/**
 * @author ryan.deschamps
 */
object App {
  def main(args: Array[String]) = {
    val writer = new PrintWriter(new File("/Users/ryandeschamps/output"))
    val file = Option(args(0)) match {
      case Some(x) => x
      case None => "/Users/ryandeschamps/edge_list" }
    val graph = Network(file)
    val eigDegree = graph.eigenVectorCentrality()
    val eigNormal = graph.normalizeValues(eigDegree).toArray
    val mat = graph.nodeSet().toArray.zip(eigNormal)
    mat.foreach(x => writer.write(x.toString + "\n"))
    writer.close()
  }
}
