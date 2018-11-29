package julienrf.course

import doodle.core._
import doodle.jvm._

object Main extends App {

  // --- Program entry point

  //draw(Circle(50)) // Complete the exercises and change this line to test your code (e.g. `draw(circles(10))`)
  //draw(spiral(9))
  //draw(sierpinski(1))
  //draw(sierpinski(2))
  //draw(sierpinski(5))

  // --- Exercises

  def circles(n: Int): Image = Circle(n)

  def spiral(n: Int): Image = {
    val radius = n * 10
    val dist = n * 40
    val angle = Angle.degrees(n * 30 % 360)
    val x = angle.cos * dist
    val y = angle.sin * dist
    val circle = Circle(radius).at(x, -y)
    if (n == 1) circle else circle on spiral(n - 1)
  }

  def equiTriangle(a: Int): Image = Triangle(a, a * (Math.sqrt(3) / 2)) fillColor Color.black

  def sierpinski(n: Int): Image = {
    if (n == 1) equiTriangle(10) else (sierpinski(n - 1) beside sierpinski(n - 1)) below sierpinski(n - 1)
  }

  // "sealed" implies exhaustive matching
  sealed trait FitnessDevice
  case class Barbell(load: Int, lenght: Int) extends FitnessDevice {
    def weight: Barbell = Barbell(load + 10, lenght + 20)

    def lighten: Barbell = Barbell(load - 10, lenght - 20)
  }

  case class Mat(width: Int, height: Int) extends FitnessDevice {
    def area: Int = width * height
    def smaller: Option[Mat] =
      if (width < 20 || height < 20) None
      else Some(Mat(width - 10, height - 10))

    def smallerButLargeEnough(mat: Mat): Option[Int] =
      mat.smaller
        .map(_.area)
        .filter(_ > 1000)
  }

  def barbellImage(barbell: Barbell): Image = {
    val bar = Rectangle(barbell.lenght, 20) fillColor Color.grey
    val weight = Rectangle(barbell.load, 60) fillColor Color.coral

    weight beside bar beside weight
  }

  def matImage(mat: Mat): Image = {
    Rectangle(mat.width, mat.height) fillColor Color.blue
  }

  def fitnessDeviceImage(device: FitnessDevice): Image = {
    device match {
      case barbell: Barbell => barbellImage(barbell)
      // case Mat(w, h) => Rectangle(w, h)
      case mat: Mat => matImage(mat)
    }
  }

  object Example {
    sealed trait GeometricShape {
      // def < val < lazy val; val is immutable and def can be reassigned
      def name: String
    }
    case class Circle(radius: Int) extends GeometricShape {
      val name = "Circle"
    }
    case class Rectangle(width: Int, height: Int) extends GeometricShape {
      val name = "Rectangle"
    }
    case class Triangle(width: Int, height: Int) extends GeometricShape {
      val name = "Triangle"
    }

    sealed trait Level
    case object Level {
      object Beginner extends Level // objects can also be matched
      object Advanced extends Level
      object Intermediate extends Level
    }


    sealed trait Expression
    case class Number(n: Int) extends Expression
    case class Add(nl: Expression, nr: Expression) extends Expression

    def eval(expr: Expression): Int = {
      expr match  {
        case Number(n) => n
        case Add(s1, s2) => eval(s1) + eval(s2)
      }
    }

    val res = eval(Add(Add(Number(1), Number(3)), Number(2)))
    println(res)
  }

  Example


  val barbell = Barbell(10, 100)
  //draw(fitnessDeviceImage(barbell))
  //draw(fitnessDeviceImage(barbell.lighten))
}