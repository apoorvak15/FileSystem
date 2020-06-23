package com.test.scala.oop.commands

import com.test.scala.oop.filesystem.State
import com.test.scala.oop.files.Directory
import com.test.scala.oop.files.DirEntry

class Mkdir(name: String) extends CreateEntry(name) {
  override def createSpecificEntry(state: State): DirEntry =
    Directory.empty(state.workingDir.path, name)
}
