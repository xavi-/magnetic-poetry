package net.magnetic.snippet

import net.liftweb._
import http._
import util._
import Helpers._
import SHtml._
import js._
import JsCmds._
import JE._

import scala.xml._
import net.magnetic.comet._

class MagneticPoetryUI {

  def renderTile(ns: NodeSeq, tup: (String, Tile)): NodeSeq = {
    val (id, tile) = tup
    
    bind("tile", ns,
      AttrBindParam("id", Text(id), "id"),
      AttrBindParam("pos", Text("left: %dpx; top: %dpx;".format(tile.x, tile.y)), "style"),
      "word" -> tile.word
    )
  }

  def render(ns: NodeSeq): NodeSeq = TileTracker.tiles.flatMap(renderTile(ns, _)).toSeq
}