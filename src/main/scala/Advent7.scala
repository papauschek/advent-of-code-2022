import java.nio.file.{Files, Path}

object Advent7:

  val MAX_DISK_SPACE = 70000000
  val MIN_UNUSED_SPACE = 30000000

  def main(args: Array[String]): Unit =
    val inputs = Files.readString(Path.of("inputs/input7.txt")).trim.linesIterator.toSeq
    val commands = getCommands(inputs)

    var state = State()
    commands.foreach {
      command => state = state.runCommand(command)
    }

    val spaceAvailable = MAX_DISK_SPACE - state.root.size
    val spaceNeeded = MIN_UNUSED_SPACE - spaceAvailable
    val smallestDir = state.root.allFolders.toSeq.filter(_.size >= spaceNeeded).minBy(_.size)
    println(smallestDir.size)


  private def getCommands(inputs: Seq[String]): Seq[Command] =
    var remainingInputs = inputs
    var commands = Seq.empty[Command]
    while (remainingInputs.nonEmpty) {
      val input = remainingInputs.head
      val outputs = remainingInputs.tail.takeWhile(!_.startsWith("$"))
      commands :+= Command(input, outputs)
      remainingInputs = remainingInputs.drop(outputs.length + 1)
    }
    commands


case class Command(input: String, outputs: Seq[String])

case class State(currentDir: String = "",
                 root: Folder = Folder()):

  def runCommand(command: Command): State =
    val parts = command.input.split(' ').drop(1).toSeq
    parts match {
      case Seq("cd", "/") =>
        copy(currentDir = "")
      case Seq("cd", "..") =>
        val newDir = currentDir.take(currentDir.lastIndexOf("/"))
        copy(currentDir = newDir)
      case Seq("cd", dirName) =>
        val newDir = (currentDir + "/" + dirName).stripMargin('/')
        copy(currentDir = newDir)
      case Seq("ls") =>
        val (outputDirs, outputFiles) = command.outputs.partition(_.startsWith("dir"))
        val folders = outputDirs.map(dir => Folder(dir.split(' ').last))
        val files = outputFiles.map {
          file =>
            val fileParts = file.split(' ').toSeq
            File(fileParts.last, fileParts.head.toLong)
        }
        copy(root = root.update(currentDir, folders, files))
    }

case class Folder(name: String = "",
                  folders: Seq[Folder] = Nil,
                  files: Seq[File] = Nil) {

  lazy val size: Long = files.map(_.size).sum + folders.map(_.size).sum

  def allFolders: Iterable[Folder] = folders ++ folders.flatMap(_.allFolders)

  def update(path: String,
             newFolders: Seq[Folder],
             newFiles: Seq[File]): Folder =
    if (path.isEmpty) {
      copy(folders = newFolders, files = newFiles)
    } else {
      val dirName = path.takeWhile(_ != '/')
      val folder = folders.find(_.name == dirName).getOrElse(throw new IllegalArgumentException(s"Not found: $dirName in $name"))
      val updatedFolder = folder.update(path.drop(dirName.length + 1), newFolders, newFiles)
      copy(folders = folders.updated(folders.indexOf(folder), updatedFolder))
    }

}

case class File(name: String, size: Long)