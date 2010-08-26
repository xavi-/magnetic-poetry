package net.magnetic.comet

import net.liftweb._
import http._
import util._
import js._
import JsCmds._
import JE._

import scala.actors._
import scala.actors.Actor._

case class MoveTiles(senderId: String, moves: Map[String, List[List[Number]]])
case class AddListener(listener: Actor)
case class RemoveListener(listener: Actor)

class Tile(var x: Int, var y: Int, var word: String)

object TileTracker extends Actor {
  val words = List("Senior", "Chief", "Vice", "Lead", "Director", "Manager", "Principle", "Executive",
                    "Officer", "Official", "Secretary", "Treasurer", "Agent", "Foreman", "President",
                    "Chairman", "Visionary", "Supervisor", "Superintendent", "Head", "General", "Partner",
                    "of", "of", "of", "of", "Information", "Financial", "Operating", "Legal", "Technical",
                    "Business", "Administrator", "Administration", "Marketing", "Relations", "Performance",
                    "Strategy", "Synergy", "Professional", "Liaison", "Consultant", "Specialist", "Operations",
                    "Engineer", "Evangelist", "and", "and", "Security", "Creative")
  var tiles = {
    val rand = new java.util.Random
    Map((0 until words.length).map { x => "word"+x -> new Tile(rand.nextInt(430), rand.nextInt(480), words(x)) }:_*)
  }
  private var listeners: List[Actor] = Nil

  this.start

  def act = loop {
    react {
      case MoveTiles(senderId, moves) =>
        for((id, posList) <- moves) {
          tiles += id -> new Tile(posList.last(0).intValue, posList.last(1).intValue, tiles(id).word)
        }
        listeners.foreach(_ ! MoveTiles(senderId, moves))
      case AddListener(listener) =>
        listeners ::= listener
      case RemoveListener(listener) =>
        listeners -= listener
      case a => println("bad track: " + a)
    }
  }
}

class PoetryCometActor extends CometActor {
  override def localSetup() {
    TileTracker ! AddListener(this)
  }

  override def localShutdown() {
    TileTracker ! RemoveListener(this)
  }

  override def render = Script(Function("moveTile", List("points"), this.jsonCall("move", JsRaw("points"))))
  
  override def highPriority() = {
    case MoveTiles(senderId, moves) if(senderId != this.uniqueId) =>
      val jsMoves = JsObj(moves.toSeq.map(x =>
              (x._1, JsArray(x._2.map(pos
                      => JsObj("x" -> pos(0).intValue, "y" -> pos(1).intValue, "time" -> pos(2).longValue)):_*))):_*)
      partialUpdate(Call("receiveUpdate", jsMoves))
  }

  override def handleJson(in: Any): JsCmd = in match {
    case JsonCmd("move", _, moves: Map[String, List[List[Number]]], _) =>
      TileTracker ! MoveTiles(this.uniqueId, moves)
      Noop
    case j => println("poo: " + j); Noop
  }
}