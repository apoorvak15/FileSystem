package com.test.scala.oop.commands
import com.test.scala.oop.filesystem.State

class Pwd extends Command {
  override def apply(state: State): State =
    state.setMessage(state.workingDir.path)
}
