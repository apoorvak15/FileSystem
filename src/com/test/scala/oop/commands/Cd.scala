package com.test.scala.oop.commands
import com.test.scala.oop.files.{DirEntry, Directory}
import com.test.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd (dir : String) extends Command {
  override def apply(state: State): State = {
  /*
  1. cd /a/b
  2. curr Dir : a , cd b/c = relative to current directory
  3. cd .
  4. cd ..
  cd a/./.././a/
   */

    //1. Find root
    val root = state.root
    val wd = state.workingDir

    //2. find the absolute path of directory I want to cd to
    val absolutePath =
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir

    //3. find the directory
    val destinationDirectory = doFindEntry (root , absolutePath)

    //4. change the state
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + " : no such directory")
    else
      State(root,destinationDirectory.asDirectory)

  }

  def doFindEntry (root: Directory, path : String) : DirEntry = {
    @tailrec
    def findEntryHelper (currentDir : Directory, path : List[String]) : DirEntry = {
      if (path.isEmpty || path.head.isEmpty) currentDir
      else if (path.tail.isEmpty) currentDir.findEntry(path.head)
      else {
        val nextDir = currentDir.findEntry(path.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory,path.tail)
      }
    }

    @tailrec
    def collapseRelativeTokens(path : List[String], resultAcc : List[String]) : List[String] = {
      if (path.isEmpty) resultAcc
      else if (".".equals(path.head)) collapseRelativeTokens(path.tail,resultAcc)
      else if ("..".equals(path.head)) {
        if (resultAcc.isEmpty) null
        else collapseRelativeTokens(path.tail, resultAcc.init)
      }else collapseRelativeTokens(path.tail, resultAcc :+ path.head)
    }

    //1. tokens
    val tokens : List[String] = path.substring(1).split(Directory.SEPARATOR).toList.filter(x => !x.equals(" ")).map(x => x.trim)

    //1.5 eliminate relative token
    val newToken = collapseRelativeTokens(tokens,List())

    //2. navigate to correct entry
    if (newToken == null)  null
     else findEntryHelper(root, newToken)
  }
}
