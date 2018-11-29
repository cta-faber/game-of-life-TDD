package julienrf.course

import doodle.core._
import doodle.jvm._

object HigherOrder {

  def stack(image: Int => Image, n: Int): Image =
    if (n == 1) image(n) else image(n) on stack(image, n - 1)

  def circle(count: Int) = stack(n => Circle(25 + 15 * n), count)

  def spiral(count: Int): Image = {
    def make(n: Int): Image = {
      val radius = n * 10
      val dist = n * 40
      val angle = Angle.degrees(n * 30 % 360)
      val x = angle.cos * dist
      val y = angle.sin * dist
      Circle(radius).at(x, -y)
    }
    stack(make, count)
  }

  def layout(op: (Image, Image) => Image, image: Int => Image, n: Int): Image =
    if (n == 1) image(n) else op(image(n), layout(op, image, n - 1))

  def layout2(op: (Image, Image) => Image, image: Int => Image, n: Int): Image = {
    (1 to n).map(image).foldLeft(Circle(0): Image)(op)
  }


  def sierpinski(count: Int): Image = ???

}
