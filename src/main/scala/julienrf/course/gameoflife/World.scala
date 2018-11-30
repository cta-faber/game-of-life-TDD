package julienrf.course.gameoflife

import doodle.core.{ Circle, Image }

case class World(grid: IndexedSeq[IndexedSeq[Cell]], rules: Rules) {
  val size: Int = grid.size

  def sizeModulo(n: Int): Int = (n + size) % size

  def getValidCell(i: Int, j: Int): Option[Cell] = {
    if (i >= - 1 && i <= size && j >= -1 && j<= size)
      Some(grid(sizeModulo(i))(sizeModulo(j)))
    else None
  }

  def getNeighbours(i: Int, j: Int): IndexedSeq[Option[Cell]] = IndexedSeq(
    getValidCell(i-1,j-1),
    getValidCell(i-1,j),
    getValidCell(i-1,j+1),
    getValidCell(i,j-1),
    getValidCell(i,j+1),
    getValidCell(i+1,j-1),
    getValidCell(i+1,j),
    getValidCell(i+1,j+1)
  )

  def getNextCellState(c: Cell, neighbours: IndexedSeq[Option[Cell]]): Cell = {
    val countAlive = neighbours.count(c => c.contains(Alive))
    Default.getRules(c, countAlive)
  }

  def getNextWorld: World = {
    val nextGrid = grid.zipWithIndex map { case (row, idxRow) =>
      row.zipWithIndex map { case (cell, idxColumn) =>
        val neighboors = getNeighbours(idxRow, idxColumn)
        getNextCellState(cell, neighboors)
      }
    }
    this.copy(grid = nextGrid)
  }

  def getNextWorldOld: World = {
    var lines: IndexedSeq[IndexedSeq[Cell]] = IndexedSeq.empty

    for (i <- 0 until size) {
      var columns: IndexedSeq[Cell] = IndexedSeq.empty

      for (j <- 0 until size) {
        val neighboors = getNeighbours(i, j)
        val nextCell = getNextCellState(grid(i)(j), neighboors)
        columns = columns :+ nextCell
      }
      lines = lines :+ columns
    }
    World(lines, Default)
  }
}

object World {

  def blinker: World = World(
    IndexedSeq(
      IndexedSeq(Dead, Dead, Dead, Dead, Dead, Dead),
      IndexedSeq(Dead, Dead, Alive, Dead, Dead, Dead),
      IndexedSeq(Alive, Dead, Alive, Dead, Dead, Dead),
      IndexedSeq(Dead, Alive, Alive, Dead, Dead, Dead),
      IndexedSeq(Dead, Dead, Dead, Dead, Dead, Dead),
      IndexedSeq(Dead, Dead, Dead, Dead, Dead, Dead)
    ),
    Default)

  def init(): World = blinker

  def toImage(w: World): Image = {
    w.grid
      .map(row => row.foldLeft(Circle(0): Image)((acc: Image, cell: Cell) => cell.draw() beside acc))
      .foldLeft(Circle(0): Image)((acc: Image, row: Image) => row above acc)
  }

  def start() {
    val clock: Events[Unit] = Events.every(200)
    val worldInit: World = World.init()

    val worlds: Events[World] = clock.fold(worldInit)((_, evolve) => evolve.getNextWorld)

    val images: Events[Image] = worlds.map(w => World.toImage(w))

    Ui.show(images)
  }
}