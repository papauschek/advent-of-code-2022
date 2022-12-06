import java.nio.file.{Files, Path}

object Advent5:

  def main(args: Array[String]): Unit =
    val input = Files.readString(Path.of("inputs/input5.txt")).linesIterator.toSeq
    val stacksInput = input.takeWhile(!_.startsWith(" 1"))
    val movesInput = input.drop(stacksInput.length + 2).filter(_.nonEmpty)
    var stacks = parseStacks(stacksInput)
    val moves = movesInput.map(parseMove)
    moves.foreach {
      move => stacks = executeMove(stacks, move)
    }
    println(stacks.map(_.crates.head).mkString)

  private def executeMove(stacks: Seq[Stack], move: Move): Seq[Stack] =
    val fromStack = stacks(move.from)
    val updatedFromStack = fromStack.copy(crates = fromStack.crates.drop(move.count))
    val toStack = stacks(move.to)
    val updatedToStack = toStack.copy(crates = fromStack.crates.take(move.count) ::: toStack.crates)
    stacks.updated(move.from, updatedFromStack).updated(move.to, updatedToStack)


  private def parseMove(line: String): Move =
    val parts = line.split(' ')
    Move(count = parts(1).toInt, from = parts(3).toInt - 1, to = parts(5).toInt - 1)

  private def parseStacks(lines: Seq[String]): Seq[Stack] =
    val stackCount = (lines.head.length + 1) / 4
    for {
      stackIndex <- 0 until stackCount
    } yield {
      val crates = lines.map(line => line(stackIndex * 4 + 1)).filter(_ != ' ')
      Stack(crates.toList)
    }

  /** @param crates in this stack, from top to bottom */
  case class Stack(crates: List[Char])

  case class Move(count: Int, from: Int, to: Int) {
    require(from != to)
  }