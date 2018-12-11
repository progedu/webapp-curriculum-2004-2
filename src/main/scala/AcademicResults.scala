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
    // 名前をキーにMapからデータを取得 Some(Some(90))、  toRight(NoUser) で Either型 Right(Some(90)) or Left(NoUser) に変換
    // .right で RightProjection(Right(Some(90)) or RightProjection(Left(Nouser))
    // 要素90　を　取り出して　pointにして返す
    (for {
      student <- findStudent(name).right  // Some(90) <- RightProjection(Right(Some(Some(90))))
      studentPoint <- student.toRight(ResultNotFound).right  // 90 <- RightProjection(Right(90))  // Some(90).toRight ===> Right(90)
    } yield Point(studentPoint)).merge    // Right(Point(90)).merge ---> Point(90)
 
  }

  def findStudent(name: String): Either[Error, Option[Int]] = {
    results.get(name).toRight(StudentNotFound)
  }

  println(find("taro")) // Point(90)
  println(find("jiro")) // ResultNotFound
  println(find("saburo")) // StudentNotFound
}
