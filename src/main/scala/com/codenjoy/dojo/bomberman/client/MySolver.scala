package com.codenjoy.dojo.bomberman.client

import com.codenjoy.dojo.bomberman.model.Elements._
import com.codenjoy.dojo.client.{Solver, WebSocketRunner}
import com.codenjoy.dojo.services.Direction.STOP
import com.codenjoy.dojo.services.PointImpl.pt
import com.codenjoy.dojo.services.{Direction, Point}

import scala.collection.JavaConverters._

class MySolver extends Solver[MyBoard] {
  var b: MyBoard = _

  override def get(b: MyBoard): String = {
    this.b = b
    nextMove.toString
  }

  /*
      Implement your logic here
   */
  def nextMove: Action = {
    val myBomberman = b.getMyBomberman
    val blastsZone = b.getBombs.flatMap(bomb => zone(bomb))
    val meatChoppersZone = b.getMeatChoppers.flatMap(chopper => zone(chopper, 2))
    val dangerZone = blastsZone ++ meatChoppersZone
    val timer1BlastsZone = b.get(BOMB_TIMER_1).asScala.toSet.flatMap((bomb: Point) => zone(bomb))
    val timer2BlastsZone = b.get(BOMB_TIMER_2).asScala.toSet.flatMap((bomb: Point) => zone(bomb))
    val meatChoppersDeathZone = b.getMeatChoppers.flatMap(chopper => zoneAround(chopper, 1))
    val deathZone = timer1BlastsZone ++ meatChoppersDeathZone
    println(s"Danger zone: $dangerZone")
    println(s"Death zone: $deathZone")
    if (timer1BlastsZone.contains(myBomberman)) {
      println("timer1BlastsZone")
      closestEscape(myBomberman, deathZone).flatMap(_.headOption).map(d => Action(d)).getOrElse(Action(STOP))
    } else if (timer2BlastsZone.contains(myBomberman)) {
      println("timer2BlastsZone")
      closestEscape(myBomberman, timer2BlastsZone)
        .filter(_.size == 2)
        .flatMap(_.headOption)
        .orElse(closestEscape(myBomberman, dangerZone).flatMap(_.headOption).filterNot(d => deathZone.contains(d.change(myBomberman))))
        .map(d => Action(d))
        .getOrElse(Action(STOP))
    } else if (meatChoppersDeathZone.contains(myBomberman)) {
      println("meatChoppersDeathZone")
      plantBombAndEscape(myBomberman, dangerZone, deathZone, NoBomb, true)
    } else if (blastsZone.contains(myBomberman)) {
      println("blastsZone")
      closestEscape(myBomberman, dangerZone).flatMap(_.headOption).filterNot(d => deathZone.contains(d.change(myBomberman))).map(d => Action(d)).getOrElse(Action(STOP))
    } else if (meatChoppersZone.contains(myBomberman)) {
      println("meatChoppersZone")
      plantBombAndEscape(myBomberman, dangerZone, deathZone, NoBomb, true)
    } else if (b.getOtherBombermans.flatMap(b => zone(b)).contains(myBomberman)) {
      println("bombermansZone")
      plantBombAndEscape(myBomberman, dangerZone, deathZone, BombBeforeMove)
    } else moveToNearest(myBomberman, b.getOtherBombermans, dangerZone)
      .orElse(moveToNearest(myBomberman, b.getMeatChoppers, dangerZone))
      .getOrElse(
        if (b.isNear(myBomberman, DESTROYABLE_WALL))
          plantBombAndEscape(myBomberman, dangerZone, deathZone, BombBeforeMove)
        else
          moveToNearest(myBomberman, b.getDestroyableWalls, dangerZone).getOrElse(Action(STOP))
      )
  }

  private def moveToNearest(myBomberman: Point, points: Set[Point], dangerZone: Set[Point]) =
    getNearest(myBomberman, points).map { case (_, path) => Action(if (dangerZone.contains(path.head.change(myBomberman))) STOP else path.head)}

  private def plantBombAndEscape(myBomberman: Point, dangerZone: Set[Point], deathZone: Set[Point], fallbackBomb: Bomb, towardsBomberman: Boolean = false) =
    closestEscape(myBomberman, dangerZone ++ zone(myBomberman), towardsBomberman)
      .flatMap(_.headOption)
      .filterNot(d => deathZone.contains(d.change(myBomberman)))
      .map(d => Action(d, BombBeforeMove))
      .orElse(closestEscape(myBomberman, dangerZone, towardsBomberman)
        .flatMap(_.headOption)
        .filterNot(d => deathZone.contains(d.change(myBomberman)))
        .map(d => Action(d)))
      .getOrElse(Action(STOP, fallbackBomb))

  private def isWall(p: Point) = {
    val e = b.getAt(p)
    e == WALL || e == DESTROYABLE_WALL
  }

  def getNearest(from: Point, points: Set[Point]): Option[(Point, Seq[Direction])] = (for {
    bomberman <- points
    path <- closestPathToPoint(from, bomberman)
  } yield (bomberman, path)).toSeq match {
    case Nil => None
    case paths@_ => Some(paths.minBy(_._2.length))
  }

  def zoneAround(point: Point, length: Int = 3): Set[Point] =
    Set((-1, 0), (1, 0), (0, -1), (0, 1)).flatMap { case (dx, dy) =>
      (1 to length)
        .map(i => pt(point.getX + i * dx, point.getY + i * dy))
        .takeWhile(p => !p.isOutOf(b.size) && !b.getImpassable.contains(p))
    }

  def zone(point: Point, length: Int = 3): Set[Point] =
    zoneAround(point, length) + point

  def closestEscape(from: Point, dangerZone: Set[Point], towardsBomberman: Boolean = false): Option[Seq[Direction]] = {
    var visited: Set[Point] = Set(from)
    var queue: List[(Point, Seq[Direction])] = List(from -> Seq.empty)
    var h = 0
    var t = 1
    while (h < t && dangerZone.contains(queue(h)._1)) {
      val currentPoint = queue(h)._1
      val currentDirections = queue(h)._2
      h += 1
      val newPoints: Seq[(Point, Direction)] = Direction.onlyDirections().asScala.map(direction => {
        val newX = direction.changeX(currentPoint.getX)
        val newY = direction.changeY(currentPoint.getY)
        pt(newX, newY) -> direction
      })
      val possiblePoints = newPoints.filter(newPoint => !b.getImpassable.contains(newPoint._1) && !visited.contains(newPoint._1))
      val sortedPoints = if (towardsBomberman) possiblePoints.sortBy { case (p, _) => getNearest(p, b.getOtherBombermans).map(_._2.length).getOrElse(100)} else possiblePoints
      sortedPoints
        .foreach(point => {
          queue = queue.:+(point._1 -> (currentDirections ++ Seq(point._2)))
          visited ++= Set(point._1)
          t += 1
        })
    }
    if (h < queue.length && !dangerZone.contains(queue(h)._1)) Some(queue(h)._2)
    else None
  }

  def closestPathToPoint(from: Point, to: Point): Option[Seq[Direction]] = {
    def tooFarFromPoint(from: Point, to: Point, point: Point): Boolean = {
      val fromX = from.getX
      val fromY = from.getY
      val toX = to.getX
      val toY = to.getY
      val pointX = point.getX
      val pointY = point.getY
      pointX < Math.min(fromX, toX) - 5 || pointX > Math.max(fromX, toX) + 5 ||
        pointY < Math.min(fromY, toY) - 5 || pointY > Math.max(fromY, toY) + 5
    }

    def bfs(from: Point, to: Point): Option[Seq[Direction]] = {
      var visited: Set[Point] = Set(from)
      var queue: List[(Point, Seq[Direction])] = List(from -> Seq.empty)
      var h = 0
      var t = 1
      while (h < t && queue(h)._1 != to) {
        val currentPoint = queue(h)._1
        val currentDirections = queue(h)._2
        h += 1
        val newPoints: Seq[(Point, Direction)] = Direction.onlyDirections().asScala.map(direction => {
          val newX = direction.changeX(currentPoint.getX)
          val newY = direction.changeY(currentPoint.getY)
          pt(newX, newY) -> direction
        })
        newPoints
          .filter(newPoint => !isWall(newPoint._1) && !visited.contains(newPoint._1) && !tooFarFromPoint(from, to, newPoint._1))
          .foreach(point => {
            queue = queue.:+(point._1 -> (currentDirections ++ Seq(point._2)))
            visited ++= Set(point._1)
            t += 1
          })
      }
      if (h < queue.length && queue(h)._1 == to) Some(queue(h)._2)
      else None
    }

    val res = bfs(from, to)
    res
  }

}

object Main extends App {

  def secret = "48fqiuyvsjomc0t2ixd9"

  def code = "8000481152348040834"

  def url = "78.141.214.125:8080"

  override def main(args: Array[String]): Unit = {
    WebSocketRunner.runClient(s"http://$url/codenjoy-contest/board/player/$secret?code=$code", new MySolver, new MyBoard)
  }
}
