object AcademicResults extends  App {

  private[this] val results = Map(
    "taro" -> Some(90),
    "jiro" -> None
  )

  sealed trait Result
  case class Point(point: Int) extends Result
  sealed trait Error extends Result
  case object StudentNotFound extends Error
  case object ResultNotFound extends Error

  def find(name: String): Result = {
    val v = (for {
      i1 <- results.get(name).toRight(StudentNotFound).right
      i2 <- i1.toRight(ResultNotFound).right
    } yield Point(i2))

    v.merge
  }

  println(find("taro")) // Point(90)
  println(find("jiro")) // ResultNotFound
  println(find("saburo")) // StudentNotFound
}
