package com.test.scala.oop.commands
import com.test.scala.oop.files.{Directory, File}
import com.test.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args : Array[String]) extends Command {
  override def apply(state: State): State = {

    if (args.isEmpty) state
    else if (args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val filename = args(args.length -1)
      val contents = createContent (args, args.length - 2)

      if (">>".equals(operator))
        doEcho(state, contents, filename,true)
      else if (">".equals(operator))
        doEcho(state,contents,filename,false)
      else
        state.setMessage(createContent(args,args.length))
    }
  }

  //top index : non inclusive
  def createContent (args : Array[String],topIndex : Int) : String = {
    @tailrec
    def createContentHelper (currentIndex : Int, acc : String) : String = {
      if (currentIndex >= topIndex) acc
      else createContentHelper(currentIndex + 1 , acc + " " + args(currentIndex))
    }
    createContentHelper(0, "")
  }

  def getRootAfterEcho(currentDir : Directory, path : List[String], contents : String, appendMode : Boolean) : Directory = {
    if (path.isEmpty) currentDir
    else if(path.tail.isEmpty){
      val dirEntry = currentDir.findEntry(path.head)
      if(dirEntry == null) currentDir.addEntry(new File(currentDir.path,path.head,contents))
      else if (dirEntry.isDirectory) currentDir
      else
        if (appendMode) currentDir.replaceEntry(path.head,dirEntry.asFile.appendContents(contents))
      else currentDir.replaceEntry(path.head,dirEntry.asFile.setContents(contents))
    } else {
      val nextDir = currentDir.findEntry(path.head).asDirectory
      val newNextDir = getRootAfterEcho(nextDir,path.tail,contents, appendMode)

      if (newNextDir == nextDir) currentDir
      else  currentDir.replaceEntry(path.head, newNextDir)
    }
  }
  def doEcho (state: State, contents : String, filename : String, appendMode : Boolean): State = {
    if (filename.contains(Directory.SEPARATOR)) state.setMessage("Echo : file name must not contain separators.")
    else {
      val newRoot : Directory = getRootAfterEcho(state.root,state.workingDir.allFoldersInPath :+ filename, contents,appendMode)
      if (newRoot == state.root)
        state.setMessage(filename + ": No such file.")
      else
        State(newRoot,newRoot.findDescendant(state.workingDir.allFoldersInPath))
    }
  }
}
