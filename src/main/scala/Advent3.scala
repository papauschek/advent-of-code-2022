import java.nio.file.{Files, Path}

object Advent3:

  def main(args: Array[String]): Unit =
    val rucksacks = Files.readString(Path.of("inputs/input3.txt")).trim.linesIterator.toSeq
    val elfGroups = rucksacks.grouped(3).toSeq
    val prioritySum = elfGroups.map(getPriorities).sum
    println(prioritySum)

  private def getPriorities(elfGroup: Seq[String]): Int =
    val duplicates = elfGroup.head.distinct.filter(item => elfGroup.forall(_.contains(item)))
    duplicates.map(getPriority).sum

  private val priorities = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  private def getPriority(item: Char): Int = priorities.indexOf(item) + 1
