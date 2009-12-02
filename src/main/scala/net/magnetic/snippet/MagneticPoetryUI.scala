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

  def renderWord(tup: (String, (Int, Int, String))): NodeSeq = {
    <div id={ tup._1 } style={ "left: %dpx; top: %dpx;".format(tup._2._1, tup._2._2) }>{ tup._2._3 }</div>
  }

  def render(xml: NodeSeq): NodeSeq = ShapeTracker.shapes.flatMap(renderWord).toSeq
}