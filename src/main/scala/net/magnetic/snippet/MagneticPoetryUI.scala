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

  def renderWord(ns: NodeSeq, tup: (String, (Int, Int, String))): NodeSeq = {
    bind("tile", ns,
      AttrBindParam("id", Text(tup._1), "id"),
      AttrBindParam("pos", Text("left: %dpx; top: %dpx;".format(tup._2._1, tup._2._2)), "style"),
      "word" -> tup._2._3
    )
  }

  def render(ns: NodeSeq): NodeSeq = ShapeTracker.shapes.flatMap(renderWord(ns, _)).toSeq
}