package aoc2022

import scala.annotation.tailrec
import scala.io.Source

object Day7 {

  val filenamePart1Sample = "2022/Day7/part1-sample.txt"
  val filenamePart1Input = "2022/Day7/part1-input.txt"

  sealed trait FileSystemItem {
    val size: Int
  }

  case class File(name: String, size: Int) extends FileSystemItem

  case class Directory(
      name: String,
      files: Vector[File],
      subDirs: Vector[Directory]
  ) extends FileSystemItem {
    override val size: Int = files.map(_.size).sum + subDirs.map(_.size).sum

    def addFiles(path: String, filesToAdd: Vector[File]): Directory = {
      val pathWithoutLeadingSlash =
        if (path.startsWith("/")) path.tail else path

      pathWithoutLeadingSlash match {
        case p if p.isEmpty =>
          // we're inside the target directory so add children
          copy(files = files ++ filesToAdd)
        case p if p.contains('/') =>
          // multiple directories in the path still so take the first one and recursively move through the path
          val (subDirName, restOfPath) =
            pathWithoutLeadingSlash.splitAt(
              pathWithoutLeadingSlash.indexOf('/')
            )

          val newSubDirs = subDirs.find(_.name == subDirName) match {
            case Some(_) => subDirs
            case None =>
              subDirs.appended(
                Directory(
                  subDirName,
                  Vector.empty[File],
                  Vector.empty[Directory]
                )
              )
          }

          copy(subDirs =
            newSubDirs.map(d =>
              if (d.name == subDirName) {
                d.addFiles(restOfPath, filesToAdd)
              } else d
            )
          )
        case _ =>
          // last dir in the path, so find or add it and add files to it
          val newDirs = subDirs.find(_.name == pathWithoutLeadingSlash) match {
            case Some(_) => subDirs
            case None =>
              subDirs.appended(
                Directory(
                  pathWithoutLeadingSlash,
                  Vector.empty[File],
                  Vector.empty[Directory]
                )
              )
          }
          copy(subDirs =
            newDirs.map(d =>
              if (d.name == pathWithoutLeadingSlash)
                d.addFiles("", filesToAdd)
              else
                d
            )
          )
      }
    }
  }

  private val commandChangeDir = "$ cd "
  private val commandListContents = "$ ls"

  def parseInput(filename: String): Directory = {
    val lines = Source.fromResource(filename).getLines().toVector

    @tailrec
    def processLine(
        index: Int,
        currentPath: String,
        currentFiles: Vector[File],
        root: Directory
    ): Directory = {
      if (index > lines.length - 1) {
        root.addFiles(currentPath, currentFiles)
      } else {
        val line = lines(index)

        line match {
          case line if line.startsWith(commandChangeDir) =>
            val newRoot = if (currentFiles.nonEmpty) {
              root.addFiles(currentPath, currentFiles)
            } else root
            val newDir = line.substring(commandChangeDir.length)
            val newPath = {
              if (newDir == "..") {
                if (currentPath.count(_ == '/') == 1)
                  "/"
                else
                  currentPath.substring(0, currentPath.lastIndexOf('/'))
              } else if (currentPath.length == 1) s"$currentPath$newDir"
              else if (currentPath.length > 1) s"$currentPath/$newDir"
              else newDir
            }
            processLine(
              index + 1,
              newPath,
              Vector.empty[File],
              newRoot
            )
          case line if line.startsWith(commandListContents) =>
            processLine(
              index + 1,
              currentPath,
              Vector.empty[File],
              root
            )
          case line if line.startsWith("dir ") =>
            processLine(
              index + 1,
              currentPath,
              currentFiles,
              root
            )
          case _ =>
            line.split(' ') match {
              case Array(sizeString, filename) =>
                processLine(
                  index + 1,
                  currentPath,
                  currentFiles.appended(File(filename, sizeString.toInt)),
                  root
                )
            }
        }
      }
    }

    processLine(
      0,
      "",
      Vector.empty[File],
      Directory("/", Vector.empty[File], Vector.empty[Directory])
    )
  }

  def getTotalSizeOfDirsUpToLimit(rootDir: Directory): Int = {
    val limit = 100000
    def getSizeIfSmallEnough(dirs: Vector[Directory]): Int = {
      dirs.foldLeft(0)((accSize, dir) => {
        if (dir.size <= limit) {
          accSize + dir.size + getSizeIfSmallEnough(dir.subDirs)
        } else {
          accSize + getSizeIfSmallEnough(dir.subDirs)
        }
      })
    }

    getSizeIfSmallEnough(rootDir.subDirs)
  }

  def getSizeOfDirectoryToDelete(rootDir: Directory): Int = {
    val totalDiskSpace = 70000000
    val requiredFreeSpace = 30000000
    val currentlyUnusedSpace = totalDiskSpace - rootDir.size
    val minSizeToDelete = requiredFreeSpace - currentlyUnusedSpace

    def getSizesOfCandidateDirs(dir: Directory): Vector[Int] = {
      if (dir.size >= minSizeToDelete) {
        dir.size +: dir.subDirs.flatMap(getSizesOfCandidateDirs)
      } else {
        Vector.empty[Int]
      }
    }

    val allSizes = getSizesOfCandidateDirs(rootDir)
    allSizes.min
  }

  def main(args: Array[String]): Unit = {
    val rootDir = parseInput(filenamePart1Input)

    println(getTotalSizeOfDirsUpToLimit(rootDir))
    println(getSizeOfDirectoryToDelete(rootDir))
  }
}
