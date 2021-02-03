/**
 * cse250.pa3.MapUtilities.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: chihotam
 * Person#: 50301678
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa3

import cse250.objects.{StreetGraph, TaxParcel}

import scala.collection.mutable
import scala.xml.XML

object MapUtilities {
  def loadIntersectionIDs(filename: String): mutable.Set[String] = {
    val xml = XML.loadFile(filename)
    val set: mutable.Set[String] = mutable.Set()
    val nodes = xml \\ "node"
    for(node <- nodes){
      set.addOne((node \ "@id").text)
    }
    set
  }

  def loadMapInfo(filename: String): mutable.Map[String, mutable.Set[String]] = {
    val xml = XML.loadFile(filename)
    val map: mutable.Map[String, mutable.Set[String]] = mutable.Map()
    var street = ""
    val nodes = xml \\ "way"
    for(node <- nodes){
      for(way <- node){
        for(tag <- way \ "tag"){
          if((tag \ "@k").text == "tiger:name_base"){
            street = (tag \ "@v").text
          }
        }
        for(nd <- way \ "nd"){
          if(map.contains((nd \ "@ref").text)){
            map((nd \ "@ref").text).addOne(street)
          }
          else{
            map.addOne((nd \ "@ref").text -> mutable.Set(street))
          }
        }
      }
     }
    map
  }

  def buildIntersectionGraph(intersectionIDs: mutable.Set[String],
                             nodeToStreetMapping: mutable.Map[String, mutable.Set[String]]): StreetGraph = {
    val streetGraph = new StreetGraph
    for(x <- intersectionIDs){
      if(nodeToStreetMapping.contains(x) && x.length > 1){
        val streets: mutable.Set[String] = nodeToStreetMapping(x)
        for(src <- streets){
          for(dest <- streets){
            if(src != dest){
              streetGraph.insertEdge(src, dest)
              streetGraph.insertEdge(dest, src)
            }
          }
        }
      }
    }
    streetGraph
  }

  //Both getTurns and getPaths were based on cse116 github BFS
  def getTurns(streetGraph: StreetGraph, src: String, dest: String): Int = {
    val explored: mutable.Queue[String] = mutable.Queue(src)
    val stack: mutable.Stack[String] = mutable.Stack(src)
    var layer = 0
    while(explored.nonEmpty){
      val current: String = explored.dequeue
      for(vertex <- streetGraph.vertices(current).edges) {
        if(!stack.contains(vertex.name)){
          explored.enqueue(vertex.name)
          stack.push(vertex.name)
        }
        if(vertex.name == dest){
          return layer+1
        }
      }
      if(streetGraph.vertices(current).edges.last.name == stack.top){
        layer += 1
      }
    }
    0
  }
  def computeFewestTurns(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Int = {
    val src = start.parcelInfo("STREET")
    val dest = end.parcelInfo("STREET")
    if(!streetGraph.vertices.contains(src) || !streetGraph.vertices.contains(dest)){
      return -1
    }
    else if(src == dest){
      return 0
    }
    getTurns(streetGraph,src,dest)
  }

  def getPath(streetGraph: StreetGraph, src: String, dest: String): Seq[String] = {
    val queue: mutable.Queue[Seq[String]] = mutable.Queue(Seq(src))
    while(queue.nonEmpty){
      val path: Seq[String] = queue.dequeue
      for(vertex <- streetGraph.vertices(path.last).edges) {
        var temp: Seq[String] = path
        if(!temp.contains(vertex.name)){
          temp = temp :+ vertex.name
          queue.enqueue(temp)
        }
        if(vertex.name == dest){
          return temp
        }
        temp.drop(1)
      }
    }
    Seq()
  }
  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Seq[String] = {
    val src = start.parcelInfo("STREET")
    val dest = end.parcelInfo("STREET")
    if(!streetGraph.vertices.contains(src) || !streetGraph.vertices.contains(dest)){
      return Seq()
    }
    getPath(streetGraph,src,dest)
  }
}
