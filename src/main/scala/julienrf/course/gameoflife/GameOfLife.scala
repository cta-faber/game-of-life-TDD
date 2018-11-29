package julienrf.course.gameoflife

import doodle.core._
import doodle.jvm.draw

object GameOfLifeTemplate extends App {

  object World {
    def blinker: World = ??? //hint: use IndexedSeq[IndexedSed[Cell]]

    def init(size: Int): World = blinker

    def toImage(w: World): Image = {
      w.grid
        .map(row => row.foldLeft(Circle(0): Image)((acc: Image, cell: Cell) => cell.draw() beside acc))
        .foldLeft(Circle(0): Image)((acc: Image, row: Image) => row above acc)
    }
  }

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
    def getRules(c: Cell, aliveNeighbours: Int): Cell = ???
  }

  case class World(grid: IndexedSeq[IndexedSeq[Cell]], rules: Rules) {
    val size: Int = ???

    def sizeModulo(n: Int): Int = ???

    def getValidCell(i: Int, j: Int): Cell = ???

    def getNeighbours(i: Int, j: Int): IndexedSeq[Cell] = ???

    def getNextCellState(c: Cell, neighbours: IndexedSeq[Cell]): Cell = ???

    def getNextWorld(): World = ???

    def start(size: Int) {
      val clock: Events[Unit] = Events.every(200)
      val worldInit: World = World.init(5)

      val worlds: Events[World] = clock.fold(worldInit)((tick, evolve) => evolve.getNextWorld())

      val images: Events[Image] = worlds.map(w => World.toImage(w))

      Ui.show(images)
    }
    start(5)
  }
}
