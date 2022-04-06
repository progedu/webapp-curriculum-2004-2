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
    (for {
      pointOpt <- findPointOpt(name).right
      result <- findResult(pointOpt).right
    } yield Point(result)).merge
  }

  def findPointOpt(name: String): Either[Error, Option[Int]] = {
    results.get(name).toRight(StudentNotFound)
  }

  def findResult(pointOpt: Option[Int]): Either[Error, Int] = pointOpt match {
    case Some(x) => Right(x)
    case None => Left(ResultNotFound)
  }



  println(find("taro")) // Point(90)
  println(find("jiro")) // ResultNotFound
  println(find("saburo")) // StudentNotFound
}
