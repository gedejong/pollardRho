import org.scalatest._

import scala.io.Source

class Problem22 extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {

  "Problem 22" should "be solved reading the file and calculating" in {

    val names = Source.fromInputStream(classOf[Problem22].getResourceAsStream("22-names.txt"))
      .getLines
      .flatMap(_.split(','))
      .map(_.replaceAll("\"", ""))
      .toSeq
      .sorted
      .zipWithIndex

    val scores = for ((name, index) ← names) yield (name.map(_ - 'A' + 1).sum * (index + 1))
    println(scores.sum)
  }
}
