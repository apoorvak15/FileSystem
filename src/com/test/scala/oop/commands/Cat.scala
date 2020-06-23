package com.test.scala.oop.commands
import com.test.scala.oop.filesystem.State

class Cat(fileName: String) extends Command {
  override def apply(state: State): State = {
    val wd = state.workingDir
    val dirEntry = wd.findEntry(fileName)
    if (dirEntry == null || dirEntry.isDirectory)
      state.setMessage(fileName + ": no such file")
    else
      state.setMessage(dirEntry.asFile.contents)
  }
}