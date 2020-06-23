package com.test.scala.oop.commands
import com.test.scala.oop.filesystem.State

trait Command {
  def apply(state: State): State

}
object Command{
  val MKDIR = "mkdir"
  val LS = "ls"
  val PWD = "pwd"
  val TOUCH = "touch"
  val CD = "cd"
  val RM = "rm"
  val ECHO = "echo"
  val CAT = "cat"
  def emptyCommand: Command = new Command {
    override def apply(state: State): State = state
  }
  def incompleteCommand (name : String) : Command = new Command {
    override def apply(state: State): State = {
      state.setMessage(name + ": incomplete command")
    }
  }

  def from(input: String) : Command = {
    val token = input.split(" ")
    if (input.isEmpty) emptyCommand
    else if (MKDIR.equals(token(0))){
      if (token.length < 2) incompleteCommand(MKDIR)
      else new Mkdir(token(1))
    } else if (LS.equals(token(0))){
      new Ls
    } else if (PWD.equals(token(0))){
      new Pwd
    } else if (TOUCH.equals(token(0))) {
      if (token.length < 2) incompleteCommand(TOUCH)
      else new Touch(token(1))
    } else if (CD.equals(token(0))) {
      if (token.length < 2) incompleteCommand(CD)
      else new Cd(token(1))
    } else if (RM.equals(token(0))) {
      if (token.length < 2) incompleteCommand(RM)
      else new Rm(token(1))
    } else if (ECHO.equals(token(0))) {
      if (token.length < 2) incompleteCommand(ECHO)
      else new Echo(token.tail)
    } else if (CAT.equals(token(0))) {
      if (token.length < 2) incompleteCommand(CAT)
      else new Cat(token(1))
    } else new UnknownCommand
  }
}