package com.test.scala.oop.commands

import com.test.scala.oop.filesystem.State

class UnknownCommand extends Command {
  override def apply(state: State): State = state.setMessage("Command not found.")
}
