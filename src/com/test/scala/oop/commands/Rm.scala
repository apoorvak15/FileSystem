package com.test.scala.oop.commands
import com.test.scala.oop.files.Directory
import com.test.scala.oop.filesystem.State

class Rm(name: String) extends Command {
  override def apply(state: State): State = {
    // get working directory
    val wd = state.workingDir

    // get absolute path
    val absolutePath =
      if (name.startsWith(Directory.SEPARATOR)) name
      else if (wd.isRoot) wd.path + name
      else wd.path + Directory.SEPARATOR + name

    // do some checks
    if (Directory.ROOT_PATH.equals(absolutePath))
      state.setMessage("Not supported to delete Root folder")
    else
      doRm(state,absolutePath)

  }
  def doRm (state: State, path : String) : State = {

    def rmHelper(currentDirectory : Directory, path : List[String]) : Directory = {
      if (path.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDir = currentDirectory.findEntry(path.head)
        if (!nextDir.isDirectory) currentDirectory
        else {
          val newNextDir = rmHelper(nextDir.asDirectory,path.tail)
          if (newNextDir == nextDir) currentDirectory
          else currentDirectory.replaceEntry(path.head,newNextDir)
        }
      }
    }
    // find the entry to remove
    // update structure like for mkdir

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot : Directory= rmHelper (state.root,tokens)

    if (newRoot == state.root)
      state.setMessage(path + " : No such file or directory")
    else
      State(newRoot, newRoot.findDescendant(state.workingDir.path.substring(1)))
  }
}
