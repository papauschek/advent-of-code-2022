import java.nio.file.{Files, Path}

object Advent4:

  def main(args: Array[String]): Unit =
    val elfPairs = Files.readString(Path.of("inputs/input4.txt")).trim.linesIterator.toSeq
    val count = elfPairs.count(isContained)
    println(count)

  private def isContained(elfPair: String): Boolean =
    val Seq(elf1, elf2) = elfPair.split(',').toSeq.map(parseRange)
    rangeOverlaps(elf1, elf2)

  private def parseRange(range: String): Range =
    val Array(start, end) = range.split('-')
    Range.inclusive(start.toInt, end.toInt)

  private def rangeOverlaps(r1: Range, r2: Range): Boolean =
    val (lower, higher) = if (r1.start < r2.start) (r1, r2) else (r2, r1)
    lower.end >= higher.start