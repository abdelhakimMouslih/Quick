case class CachedProcessedRow[A](
    originalValue: String,
    cachedValue: Option[A]
)

object CachedProcessedRow {
    def apply[A](row: String): CachedProcessedRow[A] = CachedProcessedRow(row, Option.empty[A])
    //def apply[A](row: String, cachedValue: A )= new CachedProcessedRow(row, Some(cachedValue))
}

trait ColumnPosition {
  override def toString: String
  def extractColumnValue[A, B](row: CachedProcessedRow[A] ): (CachedProcessedRow[B],Option[String])
}

class FixedLengthPosition(startsAt: Int, endsAt: Int) extends ColumnPosition {
  override def toString: String = "fixed"
  def extractColumnValue[A, B](row: CachedProcessedRow[A] ): (CachedProcessedRow[A],Option[String]) = {
    val columnValue  = row.originalValue.substring(startsAt, endsAt)
    (row, Some(columnValue))
  }
}

//class DelimitedPosition(delimiter: String, position: Int) extends ColumnPosition {
//  override def toString: String = "delimited"
//  def extractColumnValue[A,B](row: CachedProcessedRow[A]): (CachedProcessedRow[B],Option[String]) = row match {
//    case CachedProcessedRow()
//  }
//}
//
//
//def subString(positions: List[ColumnPosition], row: String): List[Option[String]] = positions match {
//    case Nil => Nil
//
//    case firstPos::rest =>
//        val processedRow = firstPos.processRaw(row)
//        for(
//            pos <- positions
//        ) yield pos.extractColumnValue(processedRow)
//}