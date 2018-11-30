package julienrf.course.gameoflife

import doodle.core.{ Color, Image, Rectangle }

sealed trait Cell {
  val square = Rectangle(30, 30)
  def draw(): Image
}
case object Alive extends Cell {
  def draw(): Image = square fillColor Color.black
}
case object Dead extends Cell {
  def draw(): Image = square fillColor Color.white
}

sealed trait Rules {
  def getRules(c: Cell, aliveNeighbours: Int): Cell
}
case object Default extends Rules {
  def getRules(c: Cell, aliveNeighbours: Int): Cell = {
    c match {
      case Alive if aliveNeighbours < 2 || aliveNeighbours > 3 => Dead
      case Dead if aliveNeighbours == 3 => Alive
      case cell => cell
    }
  }
}
