import java.nio.file.{Files, Path}

object Advent10:

  def main(args: Array[String]): Unit =
    val instructions = Files.readString(Path.of("inputs/input10.txt")).trim.linesIterator.toSeq

    var state = State(cycle = 0, register = 1)
    val cycles = state +: instructions.flatMap {
      instruction =>
        val newStates = processInstruction(state, instruction)
        state = newStates.last
        newStates
    }

    for {
      row <- cycles.grouped(40).toSeq
    } yield {
      val line = for {
        column <- row
      } yield {
        val crtPosition = column.cycle % 40
        val distance = column.register - crtPosition
        if (distance.abs <= 1) '#' else ' '
      }
      println(line.mkString)
    }


  private def processInstruction(state: State, instruction: String): Seq[State] =
    val parts = instruction.split(' ')
    parts.head match {
      case "addx" =>
        val value = parts.last.toInt
        Seq(
          state.copy(cycle = state.cycle + 1),
          state.copy(cycle = state.cycle + 2, register = state.register + value)
        )
      case "noop" =>
        Seq(state.copy(cycle = state.cycle + 1))
    }

  case class State(cycle: Int, register: Int)