import java.nio.file.{Files, Path}

object Advent6:

  private val MARKER_LENGTH = 14

  def main(args: Array[String]): Unit =
    val input = Files.readString(Path.of("inputs/input6.txt")).trim
    val firstIndex = (MARKER_LENGTH to input.length).find {
      index => input.substring(index - MARKER_LENGTH, index).distinct.length == MARKER_LENGTH
    }
    println(firstIndex)
