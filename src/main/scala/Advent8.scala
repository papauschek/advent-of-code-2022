import java.nio.file.{Files, Path}

object Advent8:

  def main(args: Array[String]): Unit =
    val trees = Files.readString(Path.of("inputs/input8.txt")).trim.linesIterator.toVector.map(_.map(_.toString.toInt).toVector)
    val (width, height) = (trees.head.length, trees.length)
    val visibility = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      val tree = trees(y)(x)
      val isLeftVisible = countUntil((0 until x).reverse.map(ox => trees(y)(ox)), tree)
      val isTopVisible = countUntil((0 until y).reverse.map(oy => trees(oy)(x)), tree)
      val isRightVisible = countUntil((x + 1 until width).map(ox => trees(y)(ox)), tree)
      val isBottomVisible = countUntil((y + 1 until height).map(oy => trees(oy)(x)), tree)
      isLeftVisible * isTopVisible * isRightVisible * isBottomVisible
    }
    println(visibility.max)


  private def countUntil(trees: Seq[Int], treeSize: Int): Int =
    val visibleTrees = trees.takeWhile(_ < treeSize)
    if (visibleTrees.length < trees.length) visibleTrees.length + 1 else visibleTrees.length