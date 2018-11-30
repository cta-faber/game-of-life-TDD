package julienrf.course.gameoflife

import org.scalatest.{ MustMatchers, WordSpec }

class GameOfLife extends WordSpec with MustMatchers {

  val grid: IndexedSeq[IndexedSeq[Cell]] = IndexedSeq(IndexedSeq(Alive, Alive, Dead), IndexedSeq(Alive, Alive, Dead), IndexedSeq(Alive, Alive, Dead))
  val world: World = World(grid, Default)


  "getValidCell" should {

    "return some cell when coordinates are in the grid" in {
      // Given
      val x = 2
      val y = 2

      // When
      val ceilOpt: Option[Cell] = world.getValidCell(x, y)

      // Then
      ceilOpt mustBe defined
      ceilOpt.get mustBe Dead
    }

    "return some cell when coordinates are in grid limits plus one" in {
      // Given
      val x = -1
      val y = 3

      // When
      val ceilOpt: Option[Cell] = world.getValidCell(x, y)

      // Then
      ceilOpt mustBe defined
      ceilOpt.get mustBe Alive
    }

    "return none when coordinates are not in the grid" in {
      // Given
      val x = 12
      val y = 22

      // When
      val ceilOpt: Option[Cell] = world.getValidCell(x, y)

      // Then
      ceilOpt mustBe None
    }
  }

  "getNeighbours" should {

    "return the list of every neighbours when not on the edge of the grid" in {
      //given
      val x = 1
      val y = 1
      val expected: IndexedSeq[Option[Cell]] = IndexedSeq(Some(Alive), Some(Alive), Some(Dead), Some(Alive), Some(Dead), Some(Alive), Some(Alive), Some(Dead))

      //when
      val neighbours: IndexedSeq[Option[Cell]] = world.getNeighbours(x, y)
      //then
      neighbours.size mustBe 8
      neighbours.mustBe(expected)
    }

    "return the list of every neighbours when on the edge of the grid" in {
      //given
      val x = 0
      val y = 2
      val expected: IndexedSeq[Option[Cell]] = IndexedSeq(Some(Alive), Some(Dead), Some(Alive), Some(Alive), Some(Alive), Some(Alive), Some(Dead), Some(Alive))

      //when
      val neighbours: IndexedSeq[Option[Cell]] = world.getNeighbours(x, y)
      //then
      neighbours.size mustBe 8
      neighbours.mustBe(expected)
    }

    "return the list of every neighbours when not on the grid" in {
      //given
      val x = -1
      val y = 1
      val expected: IndexedSeq[Option[Cell]] = IndexedSeq(None, None, None, Some(Alive), Some(Dead), Some(Alive), Some(Alive), Some(Dead))

      //when
      val neighbours: IndexedSeq[Option[Cell]] = world.getNeighbours(x, y)
      //then
      neighbours.size mustBe 8
      neighbours.mustBe(expected)
    }
  }

  "getRules" should {

    "return a dead cell when it is alive and it has more than 3 alive neighboors" in {
      //given
      val cell = Alive
      val aliveNeighbours = 4
      val expected = Dead

      //when
      val resultCell = Default.getRules(cell, aliveNeighbours)
      //then
      resultCell mustBe expected
    }

    "return a living cell when it is alive and has 2 or 3 alive neighboors " in {
      //given
      val cell = Alive
      val aliveNeighbours = 3
      val expected = Alive

      //when
      val resultCell = Default.getRules(cell, aliveNeighbours)
      //then
      resultCell mustBe expected
    }

    "return a cell alive when it is dead and has exacly 3 alive neighboors" in {
      //given
      val cell = Dead
      val aliveNeighbours = 3
      val expected = Alive

      //when
      val resultCell = Default.getRules(cell, aliveNeighbours)
      //then
      resultCell mustBe expected
    }

    "return a dead cell when it is alive and has less than 2 alive neighboors" in {
      //given
      val cell = Alive
      val aliveNeighbours = 1
      val expected = Dead

      //when
      val resultCell = Default.getRules(cell, aliveNeighbours)
      //then
      resultCell mustBe expected
    }
  }

  "getNextWorld" should {
    "return the next world" in {
      val actual: World = world.getNextWorld
      val expected = IndexedSeq(IndexedSeq(Dead, Dead, Dead),IndexedSeq(Dead, Dead, Dead),IndexedSeq(Dead, Dead, Dead))
      actual.grid mustBe expected
    }
  }
}
