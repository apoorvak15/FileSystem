package com.test.scala.oop.files

import com.test.scala.oop.filesystem.FileSystemException

import scala.annotation.tailrec

class Directory(override val parentPath: String, override val name : String, val contents: List[DirEntry]) extends DirEntry(parentPath, name){
  def findDescendant(path: List[String]): Directory = {
    if (path.isEmpty) this
    else findEntry(path.head).asDirectory.findDescendant(path.tail)
  }

  def findDescendant (relativePath : String) : Directory = {
    if (relativePath.isEmpty || relativePath.trim.equals("")) this
    else findDescendant(relativePath.split(Directory.SEPARATOR).toList)
  }

  def removeEntry (entryName: String) : Directory =
    if (!hasEntry(entryName)) this
    else new Directory(parentPath, name, contents.filter(x => ! x.name.equals(entryName.trim)))

  def hasEntry (name : String) : Boolean = findEntry(name) != null

  def allFoldersInPath : List[String] =
    path.substring(1).split(Directory.SEPARATOR).toList.filter(x => !x.equals(" "))
  // /a/b/c/d  ==  List["a","b","c","d"]

  def addEntry(newEntry: DirEntry) : Directory = new Directory(parentPath,name,contents :+ newEntry)

  def findEntry(entryName: String) : DirEntry = {
    @tailrec
    def findEntryHelper(name: String, contentList : List[DirEntry]) : DirEntry = {
      if (contentList.isEmpty) null
      else if (contentList.head.name.equals(name.trim)) contentList.head
      else findEntryHelper(name,contentList.tail)
    }
    findEntryHelper(entryName,contents)
  }
  def replaceEntry (entryNam: String , newEntry : DirEntry) : Directory =
    new Directory(parentPath,name, contents.filter(e => !e.name.equals(entryNam)) :+ newEntry)


  def isRoot : Boolean = parentPath.isEmpty

  def isDirectory : Boolean = true
  def isFile : Boolean = false

  override def asDirectory: Directory = this

  override def asFile: File = throw new FileSystemException("A directory cannot be converted to a file.")

  override def getType: String = "Directory"
}

object Directory{
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def empty(parentPath: String, name: String) = {
    new Directory(parentPath, name, List())
  }
  def ROOT : Directory = Directory.empty(""," ")
}