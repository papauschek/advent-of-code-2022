import java.nio.file.{Files, Path}

object Advent9:

  def main(args: Array[String]): Unit =
    val movements = Files.readString(Path.of("inputs/input9.txt")).trim.linesIterator.toSeq
    val initialLocation = Location(0, 0)
    var state = State(Vector.fill(10)(initialLocation), visitedLocations = Set(initialLocation))

    movements.foreach {
      movement =>
        state = moveLocation(state, movement)
    }
    println(state.visitedLocations.size)

  private def moveLocation(initialState: State, movement: String): State =
    val delta = deltaByDirection(movement(0))
    val distance = movement.substring(2).toInt
    var state = initialState
    (0 until distance).foreach {
      _ => state = moveLocation(state, delta)
        println(state.rope)
    }
    state

  private val deltaByDirection = Map(
    'U' -> Location(0, -1),
    'D' -> Location(0, 1),
    'L' -> Location(-1, 0),
    'R' -> Location(1, 0)
  )

  private def moveLocation(state: State, delta: Location): State =
    val newHead = state.rope.head.plus(delta.x, delta.y)
    var previousHead = newHead
    val newTails = state.rope.tail.map {
      tail =>
        val (dx, dy) = (previousHead.x - tail.x, previousHead.y - tail.y)
        val (mx, my) =
          if (dx.abs >= 2 || dy.abs >= 2) {
            (dx.sign, dy.sign)
          } else {
            (0, 0)
          }
        val newTail = tail.plus(mx, my)
        previousHead = newTail
        newTail
    }

    state.copy(
      rope = newHead +: newTails,
      visitedLocations = state.visitedLocations + newTails.last
    )


  case class State(rope: Vector[Location], visitedLocations: Set[Location])

  case class Location(x: Int, y: Int):

    def plus(dx: Int, dy: Int): Location = copy(x = x + dx, y = y + dy)