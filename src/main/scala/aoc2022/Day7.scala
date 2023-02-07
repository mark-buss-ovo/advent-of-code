package aoc2022

import scala.annotation.tailrec
import scala.io.Source

object Day7 {

  private val filenamePart1Sample = "2022/Day7/part1-sample.txt"
  private val filenamePart1Input = "2022/Day7/part1-input.txt"

  private case class File(name: String, size: Int)

  private case class Directory(
      name: String,
      files: Vector[File],
      subDirs: Vector[Directory]
  ) {
    val size: Int = files.map(_.size).sum + subDirs.map(_.size).sum

    private def addToSubDirsIfMissing(subDirName: String): Vector[Directory] = {
      subDirs.find(_.name == subDirName) match {
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
    }

    private def addFilesToSubDir(
        subDirName: String,
        restOfPath: String,
        filesToAdd: Vector[File]
    ): Directory = {
      val newSubDirs = addToSubDirsIfMissing(subDirName)

      copy(subDirs =
        newSubDirs.map(d =>
          if (d.name == subDirName) {
            d.addFiles(restOfPath, filesToAdd)
          } else d
        )
      )
    }

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

          addFilesToSubDir(subDirName, restOfPath, filesToAdd)
        case _ =>
          // last dir in the path, so find or add it and add files to it
          addFilesToSubDir(pathWithoutLeadingSlash, "", filesToAdd)
      }
    }
  }

  private val commandChangeDir = "$ cd "
  private val commandListContents = "$ ls"

  private def parseInput(filename: String): Directory = {
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

  private def getTotalSizeOfDirsUpToLimit(filename: String): Int = {
    val rootDir = parseInput(filename)
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

  private def getSizeOfDirectoryToDelete(filename: String): Int = {
    val rootDir = parseInput(filename)
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
    println(getTotalSizeOfDirsUpToLimit(filenamePart1Sample))
    println(getTotalSizeOfDirsUpToLimit(filenamePart1Input))
    println(getSizeOfDirectoryToDelete(filenamePart1Sample))
    println(getSizeOfDirectoryToDelete(filenamePart1Input))
  }
}
