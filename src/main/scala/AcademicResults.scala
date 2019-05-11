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
      pointOpt <- results.get(name).toRight(StudentNotFound) // 点数をOption[Int]で取得。Noneの時はユーザーが存在しない
      point <- pointOpt.toRight(ResultNotFound) // 点数がNoneの場合は点数が存在しない
    } yield Point(point)).merge // 点数の取得まで完了できたらPointを返す。それ以外はエラーが返る。
  }

  println(find("taro")) // Point(90)
  println(find("jiro")) // ResultNotFound
  println(find("saburo")) // StudentNotFound
}
