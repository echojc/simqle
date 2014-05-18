package sh.echo.simqle

import java.sql.ResultSet

trait ColumnMapper[T] {
  def fromResultSet(rs: ResultSet, index: Int): T
  def fromResultSet(rs: ResultSet, column: String): T
}

object ColumnMapper {

  implicit object IntColumnMapper extends ColumnMapper[Int] {
    def fromResultSet(rs: ResultSet, index: Int): Int = {
      rs.getInt(index)
    }
    def fromResultSet(rs: ResultSet, column: String): Int = {
      rs.getInt(column)
    }
  }

  implicit object StringColumnMapper extends ColumnMapper[String] {
    def fromResultSet(rs: ResultSet, index: Int): String = {
      rs.getString(index)
    }
    def fromResultSet(rs: ResultSet, column: String): String = {
      rs.getString(column)
    }
  }
}
