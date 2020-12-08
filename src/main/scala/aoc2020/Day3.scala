package aoc2020

import scala.annotation.tailrec

object Day3 extends Day(3){
  case class Coord(x: Int, y: Int)

  def traverseMap(map: List[List[Char]], increment: Coord): Int = {
    val (maxX, maxY) = (map.head.size, map.size - 1)
    @tailrec
    def recursePoint(currentPoint: Coord = Coord(0, 0), currentCount: Int = 0): Int =
      currentPoint match {
        case Coord(_, y) if y > maxY => currentCount
        case Coord(x, y) =>
          recursePoint(Coord((x + increment.x) % maxX, y + increment.y), if (map(y)(x) == '#') currentCount + 1 else currentCount)
      }
    recursePoint()
  }

  def solution(lines: List[String]): Unit = {
    val secondPartCoords = List(
      Coord(1, 1),
      Coord(3, 1),
      Coord(5, 1),
      Coord(7, 1),
      Coord(1, 2)
    )
    val dayThreeInput = lines.map(_.toList)
    println(s"No of trees hit: ${traverseMap(dayThreeInput, Coord(3, 1))}")
    println(s"Total number of trees hit part 2: ${secondPartCoords.map(traverseMap(dayThreeInput, _)).map(BigInt(_)).product}")
  }
}
