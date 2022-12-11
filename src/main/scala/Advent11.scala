import java.nio.file.{Files, Path}

object Advent11:

  def main(args: Array[String]): Unit =
    val inputMonkeys = Files.readString(Path.of("inputs/input11.txt")).trim.split("\\n\\n").toSeq
    val monkeys = inputMonkeys.map(parseMonkey).toVector
    var state = State(monkeys)
    (0 until 10000).foreach {
      round =>

        if (round == 1 || round == 20 || round % 1000 == 0) {
          println((round, state.monkeys.map(_.activeCount).mkString(", ")))
        }

        state.monkeys.indices.foreach {
          monkeyIndex =>
            state = monkeyTurn(state, monkeyIndex)
        }
    }
    val bestMonkeys = state.monkeys.sortBy(_.activeCount).reverse.take(2)
    val Seq(m1, m2) = bestMonkeys.map(_.activeCount.toLong)
    println((m1, m2, m1 * m2))

  private def monkeyTurn(state: State, monkeyIndex: Int): State =
    val monkey = state.monkeys(monkeyIndex)

    var newMonkeys = state.monkeys
    val dividerProduct = newMonkeys.map(_.testDivider).product
    monkey.items.foreach {
      item =>
        val worryLevel = monkey.operation.execute(item) % dividerProduct
        require(worryLevel > 0, item.toString)
        val isDivisible = (worryLevel % monkey.testDivider) == 0
        val targetMonkeyIndex = if (isDivisible) monkey.trueTarget else monkey.falseTarget
        val targetMonkey = newMonkeys(targetMonkeyIndex)
        newMonkeys = newMonkeys.updated(targetMonkeyIndex, targetMonkey.copy(
          items = targetMonkey.items :+ worryLevel
        ))
    }
    newMonkeys = newMonkeys.updated(monkeyIndex, monkey.copy(
      items = Vector.empty,
      activeCount = monkey.activeCount + monkey.items.length
    ))
    state.copy(monkeys = newMonkeys)

  private def parseMonkey(input: String): Monkey =
    val lines = input.linesIterator.toVector.map(_.trim)
    val startItems = lines(1).split(' ').drop(2).map(_.takeWhile(_.isDigit).toLong).toVector
    val operationParts = lines(2).split(' ').drop(3).toSeq
    val operation = operationParts match {
      case Seq("old", "*", "old") => SquareOperation
      case Seq("old", "*", multiplier) => MultiplyOperation(multiplier.toInt)
      case Seq("old", "+", value) => AddOperation(value.toInt)
    }
    val testDivider = lines(3).split(' ').last.toInt
    val trueTarget = lines(4).split(' ').last.toInt
    val falseTarget = lines(5).split(' ').last.toInt
    Monkey(startItems, operation, testDivider, falseTarget, trueTarget)

  case class State(monkeys: Vector[Monkey])

  case class Monkey(items: Vector[Long],
                    operation: Operation,
                    testDivider: Int,
                    falseTarget: Int,
                    trueTarget: Int,
                    activeCount: Int = 0):
    override def toString: String = s"${items.mkString(", ")} ($activeCount)"

  trait Operation:
    def execute(input: Long): Long

  case class MultiplyOperation(multiplier: Int) extends Operation:
    override def execute(input: Long): Long = input * multiplier

  case class AddOperation(value: Int) extends Operation:
    override def execute(input: Long): Long = input + value

  case object SquareOperation extends Operation:
    override def execute(input: Long): Long = input * input

