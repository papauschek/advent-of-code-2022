import java.nio.file.{Files, Path}
import Shape._
import Result._

object Advent2:

  def main(args: Array[String]): Unit =
    val strategies = Files.readString(Path.of("inputs/input2.txt")).trim.linesIterator.toSeq
    val pointsSum = strategies.map(getPoints).sum
    println(pointsSum)

  private def getPoints(strategy: String): Int =
    val opponentShape = Shape.byChar(strategy(0))
    val desiredResult = Result.byChar(strategy(2))
    val selfShape = getShapeForResult(opponentShape, desiredResult)
    desiredResult.resultId + selfShape.shapeId

  private def getShapeForResult(opponent: Shape, result: Result): Shape =
    if (result == Draw) {
      // use same shape as opponent for draw
      opponent
    } else if (result == Win) {
      // the shape after the opponent shape is the winning one
      Shape.fromOrdinal((opponent.ordinal + 1) % 3)
    } else {
      // the shape before the opponent shape is the losing one
      Shape.fromOrdinal((opponent.ordinal + 2) % 3)
    }


enum Shape(val shapeId: Int):
  case Rock extends Shape(1)
  case Paper extends Shape(2)
  case Scissors extends Shape(3)

object Shape:
  val byChar: Map[Char, Shape] = Map(
    'A' -> Rock,
    'B' -> Paper,
    'C' -> Scissors
  )

enum Result(val resultId: Int):
  case Loss extends Result(0)
  case Draw extends Result(3)
  case Win extends Result(6)

object Result:
  val byChar: Map[Char, Result] = Map(
    'X' -> Loss,
    'Y' -> Draw,
    'Z' -> Win
  )

