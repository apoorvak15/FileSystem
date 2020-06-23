package com.test.scala.oop.filesystem

import com.test.scala.oop.files.Directory

class State (val root: Directory, val workingDir: Directory, val outputPrevCmd : String) {

  def show : Unit = {
    println(outputPrevCmd)
    print(State.SHELL_TOKEN)
  }

  def setMessage(message : String) : State =
    State(root,workingDir,message)
}

object State{
  val SHELL_TOKEN = "$ "
  def apply(root: Directory, workingDir: Directory, outputPrevCmd: String = ""): State = new State(root, workingDir, outputPrevCmd)
}