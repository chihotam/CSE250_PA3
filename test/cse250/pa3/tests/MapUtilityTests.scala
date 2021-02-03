/**
 * cse250.pa3.tests.MapUtilityTests.scala
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
package cse250.pa3.tests

import org.scalatest.{BeforeAndAfter, FlatSpec}
import cse250.pa3.MapUtilities

import scala.collection.mutable
import cse250.objects.{StreetGraph, TaxParcel}


class MapUtilityTests extends FlatSpec with BeforeAndAfter {

  // Your tests for problem 1 should be contained under this header.
  behavior of "MapUtilityTests 1(a)"
  it should "build a correct graph with the given inputs" in {
    val map = MapUtilities
    val intersectionIDs: mutable.Set[String] = mutable.Set("1001","1002","1003","1004","1005","1006","1007")
    val nodeToStreetMapping: mutable.Map[String, mutable.Set[String]] = mutable.Map(
      "1001" -> mutable.Set("AA","BB"),
      "1002" -> mutable.Set("BB","CC"),
      "1003" -> mutable.Set("CC","DD","EE"),
      "1004" -> mutable.Set("DD","FF"),
      "1005" -> mutable.Set("EE","FF"),
      "1006" -> mutable.Set("GG"),
      "1008" -> mutable.Set("AA","FF"))
    val output: StreetGraph = map.buildIntersectionGraph(intersectionIDs, nodeToStreetMapping)
    val graph: StreetGraph = new StreetGraph
    graph.insertEdge("AA","BB")
    graph.insertEdge("BB","AA")
    graph.insertEdge("BB","CC")
    graph.insertEdge("CC","BB")
    graph.insertEdge("CC","DD")
    graph.insertEdge("DD","CC")
    graph.insertEdge("CC","EE")
    graph.insertEdge("EE","CC")
    graph.insertEdge("DD","EE")
    graph.insertEdge("EE","DD")
    graph.insertEdge("DD","FF")
    graph.insertEdge("FF","DD")
    graph.insertEdge("EE","FF")
    graph.insertEdge("FF","EE")
    assert(output.edges == graph.edges)
    //println(output.edges)
    //assert(output.vertices == graph.vertices)
  }

  behavior of "MapUtilityTests 1(b)"
  it should "return 0 as the fewest turns" in {
    val map = MapUtilities
    val graph: StreetGraph = new StreetGraph
    graph.insertEdge("AA","BB")
    graph.insertEdge("BB","AA")
    graph.insertEdge("BB","CC")
    graph.insertEdge("CC","BB")
    graph.insertEdge("CC","DD")
    graph.insertEdge("DD","CC")
    graph.insertEdge("CC","EE")
    graph.insertEdge("EE","CC")
    graph.insertEdge("DD","EE")
    graph.insertEdge("EE","DD")
    graph.insertEdge("DD","FF")
    graph.insertEdge("FF","DD")
    graph.insertEdge("EE","FF")
    graph.insertEdge("FF","EE")
    val tax1: TaxParcel = new TaxParcel
    tax1.parcelInfo += ("STREET" -> "AA")
    assert(map.computeFewestTurns(graph,tax1,tax1) == 0)
  }
  it should "return -1 as the fewest turns" in {
    val map = MapUtilities
    val graph: StreetGraph = new StreetGraph
    graph.insertEdge("AA","BB")
    graph.insertEdge("BB","AA")
    graph.insertEdge("BB","CC")
    graph.insertEdge("CC","BB")
    graph.insertEdge("CC","DD")
    graph.insertEdge("DD","CC")
    graph.insertEdge("CC","EE")
    graph.insertEdge("EE","CC")
    graph.insertEdge("DD","EE")
    graph.insertEdge("EE","DD")
    graph.insertEdge("DD","FF")
    graph.insertEdge("FF","DD")
    graph.insertEdge("EE","FF")
    graph.insertEdge("FF","EE")
    val tax1: TaxParcel = new TaxParcel
    tax1.parcelInfo += ("STREET" -> "AA")
    val tax2: TaxParcel = new TaxParcel
    tax2.parcelInfo += ("STREET" -> "ZZ")
    assert(map.computeFewestTurns(graph,tax1,tax2) == -1)
  }
  it should "return 4 as the fewest turns and correct shortest path" in {
    val map = MapUtilities
    val graph: StreetGraph = new StreetGraph
    graph.insertEdge("AA","BB")
    graph.insertEdge("BB","AA")
    graph.insertEdge("BB","CC")
    graph.insertEdge("CC","BB")
    graph.insertEdge("CC","DD")
    graph.insertEdge("DD","CC")
    graph.insertEdge("CC","EE")
    graph.insertEdge("EE","CC")
    graph.insertEdge("DD","EE")
    graph.insertEdge("EE","DD")
    graph.insertEdge("DD","FF")
    graph.insertEdge("FF","DD")
    graph.insertEdge("EE","FF")
    graph.insertEdge("FF","EE")
    graph.insertEdge("ZZ","FF")
    graph.insertEdge("FF","ZZ")
    graph.insertEdge("EE","ZZ")
    graph.insertEdge("ZZ","EE")
    val tax1: TaxParcel = new TaxParcel
    tax1.parcelInfo += ("STREET" -> "AA")
    val tax2: TaxParcel = new TaxParcel
    tax2.parcelInfo += ("STREET" -> "ZZ")
    assert(map.computeFewestTurns(graph,tax1,tax2) == 4)
    assert(map.computeFewestTurnsList(graph,tax1,tax2) == Seq("AA","BB","CC","EE","ZZ"))
  }

  // ^^^
  behavior of "Other Functionality"
  it should "..." in {
    //val map = MapUtilities
    //map.loadIntersectionIDs("data/buffalo-map.xml")
    //map.loadMapInfo("data/buffalo-map.xml")
  }
}

