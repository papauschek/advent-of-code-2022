import java.nio.file.{Files, Path}

object Advent1:

  def main(args: Array[String]): Unit =
    val input = Files.readString(Path.of("inputs/input1.txt")).trim
    val elfs = input.split("\n\n").toSeq
    val elfsWithCalories = elfs.map(_.linesIterator.map(_.toInt).sum)
    val topThreeSum = elfsWithCalories.sorted.reverse.take(3).sum
    println(topThreeSum)

