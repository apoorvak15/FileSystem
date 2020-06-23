package com.test.scala.oop.commands
import com.test.scala.oop.files.{DirEntry, File}
import com.test.scala.oop.filesystem.State

class Touch(name: String)  extends CreateEntry (name){
  override def createSpecificEntry(state: State): DirEntry =
    File.empty(state.workingDir.path,name)

}
