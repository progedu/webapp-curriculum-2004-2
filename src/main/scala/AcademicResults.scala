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
      pointOpt <- getPointOpt(name).right
      point <-  getPoint(pointOpt).right
    } yield Point(point)).merge
  }

  def getPointOpt(name: String): Either[Error, Option[Int]] = {
    results.get(name).toRight(StudentNotFound)
  }

  def getPoint(pointOpt: Option[Int]): Either[Error, Int] ={
    pointOpt.toRight(ResultNotFound)
  }

  println(find("taro")) // Point(90)
  println(find("jiro")) // ResultNotFound
  println(find("saburo")) // StudentNotFound
}
