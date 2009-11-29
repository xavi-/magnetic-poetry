package net.magnetic.comet

import net.liftweb._
import http._
import util._
import js._
import JsCmds._
import JE._

import scala.actors._
import scala.actors.Actor._

case class BroadcastPlaces(senderId: String)
case class ShapePlaces(senderId: String, places: Map[String, (Int, Int, String)])
case class MoveShapes(senderId: String, moves: Map[String, List[List[Number]]])
case class AddListener(listener: Actor)
case class RemoveListener(listener: Actor)

object ShapeTracker extends Actor {
  private val words = List("Senior", "Chief", "Vice", "Lead", "Director", "Manager", "Principle", "Executive",
                           "Officer", "Official", "Secretary", "Treasurer", "Agent", "Foreman", "President",
                           "Chairman", "Visionary", "Supervisor", "Superintendent", "Head", "General", "Partner",
                           "of", "of", "of", "of", "Information", "Financial", "Operating", "Legal", "Technical",
                           "Business", "Administrator", "Administration", "Marketing", "Relations", "Performance",
                           "Strategy", "Synergy", "Professional", "Liaison", "Consultant", "Specialist", "Operations",
                           "Engineer", "Evangelist", "and", "and", "Security", "Creative")
  private val rand = new java.util.Random
  private var shapes = Map((0 until words.length).map(x => ("word"+x, (rand.nextInt(430), rand.nextInt(480), words(x)))):_*)
  private var listeners: List[Actor] = Nil

  this.start

  def act = loop {
    react {
      case BroadcastPlaces(senderId) =>
        listeners.foreach(_ ! ShapePlaces(senderId, shapes))
      case MoveShapes(senderId, moves) =>
        for((name, posList) <- moves) {
          shapes += name -> (posList.last(0).intValue, posList.last(1).intValue, shapes(name)._3)
        }
        listeners.foreach(_ ! MoveShapes(senderId, moves))
      case AddListener(listener) =>
        listeners ::= listener
      case RemoveListener(listener) =>
        listeners -= listener
      case a => println("bad track: " + a)
    }
  }
}

class MagneticPoetryDisplay extends CometActor {
  override def localSetup() {
    ShapeTracker ! AddListener(this)
  }

  override def localShutdown() {
    ShapeTracker ! RemoveListener(this)
  }

  override def render =
    Script(Function("moveShape", List("points"), this.jsonCall("move", JsRaw("points"))) &
           Function("getPlacements", Nil, this.jsonCall("places")))

  override def highPriority() = {
    case ShapePlaces(senderId, places) if(senderId == this.uniqueId) =>
      val jsPlaces = JsObj(places.toSeq.map(x => (x._1, JsObj("x" -> x._2._1, "y" -> x._2._2, "word" -> x._2._3))):_*)
      partialUpdate(Call("receivePlacements", jsPlaces))
    case MoveShapes(senderId, moves) if(senderId != this.uniqueId) =>
      val jsMoves = JsObj(moves.toSeq.map(x =>
              (x._1, JsArray(x._2.map(pos
                      => JsObj("x" -> pos(0).intValue, "y" -> pos(1).intValue, "time" -> pos(2).longValue)):_*))):_*)
      partialUpdate(Call("receiveUpdate", jsMoves))
  }

  override def handleJson(in: Any): JsCmd = in match {
    case JsonCmd("move", _, moves: Map[String, List[List[Number]]], _) =>
      ShapeTracker ! MoveShapes(this.uniqueId, moves)
      Noop
    case JsonCmd("places", _, _, _) =>
      ShapeTracker ! BroadcastPlaces(this.uniqueId)
      Noop
    case j => println("poo: " + j); Noop
  }
}