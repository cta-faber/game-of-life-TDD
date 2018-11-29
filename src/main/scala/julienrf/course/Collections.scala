package julienrf.course

import doodle.core._
//import doodle.jvm.draw

object Collections {

  /**
   * @return a sequence whose elements are the elements of `xs` incremented by one
   */
  def incRec(xs: Seq[Int]): Seq[Int] = {
    xs match {
      case Nil => Nil
      case x +: xs => (x + 1) +: incRec(xs)
    }
  }

  def inc(xs: Seq[Int]): Seq[Int] = xs.map(_ + 1)


    /**
   * @return a sequence whose elements are the length of the elements of `ss`
   */
  def lengthsRec(ss: Seq[String]): Seq[Int] = {
    ss match {
      case Nil => Nil
      case s +: sl => s.length +: lengthsRec(sl)
    }
  }

  def lengths(ss: Seq[String]): Seq[Int] = ss.map(_.length)

  def sumRec(xs: Seq[Int]): Int = {
    xs match {
      case Nil => 0
      case s +: sl => s + sumRec(sl)
    }
  }

  def sum(xs: Seq[Int]): Int = xs.foldLeft(0)((acc, x) => x + acc)

  def productRec(xs: Seq[Int]): Int = {
    xs match {
      case Nil => 0
      case s +: Nil => s
      case s +: sl => s * productRec(sl)
    }
  }

  def product(xs: Seq[Int]): Int = xs.foldLeft(1)((acc, x) => x * acc)

    /**
   * Define a method that takes a sequence of Mat and returns their areas if this one is greater than 1000
   */
  // def largeEnough(mats: Seq[Mat]): Seq[Int] = ???

  /**
   * Returns `n` concentric circles
   */
  def circles(n: Int): Seq[Circle] = (1 to n).map(i => Circle(i * 10 * 30))

  /**
   * Returns `n` concentric circles (recursive implementation)
   */
  def circlesRec(n: Int): Seq[Circle] = {
    if (n == 1) Seq(Circle(40))
    else Circle(n * 10 + 30) +: circlesRec(n - 1)
  }


  def spiral(n: Int): Seq[Image] = (1 to n).map(i => makeSpiral(i))

  def makeSpiral(n: Int): Image = {
    val radius = n * 10
    val dist = n * 40
    val angle = Angle.degrees(n * 30 % 360)
    val x = angle.cos * dist
    val y = angle.sin * dist
    Circle(radius).at(x, -y)
  }

  def spiralRec(n: Int): Seq[Image] = ???

  val imageZero = Circle(0)
  def stack(images: Seq[Image]): Image = images.foldLeft(imageZero: Image)((acc, img) => img on acc)

  def stackRec(images: Seq[Image]): Image = images match {
    case Nil => Circle(0)
    case img +: tail => img on stackRec(tail)
  }

  import Main.Mat
  def largeEnoughRec(mats: Seq[Mat]): Seq[Int] = mats match {
    case Nil => Nil
    case Mat(w, h) +: tail => {
      val area = w * h
      if (area > 1000)
        (area +: largeEnoughRec(tail))
      else
        largeEnoughRec(tail)
    }
  }

  def largeEnough(mats: Seq[Mat]): Seq[Int] = {
    mats
      .map(mat => mat.width * mat.height)
      .filter(area => area > 1000)
  }

  def layout(op: (Image, Image) => Image, images: Seq[Image]): Image = images.foldLeft(imageZero: Image)(op)

}
