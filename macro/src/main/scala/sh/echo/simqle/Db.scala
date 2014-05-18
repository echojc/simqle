package sh.echo.simqle

import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.sql.Statement

case class Db(connString: String) {

  def withConnection[T](f: Connection ⇒ T): T = {
    var conn: Connection = null
    try {
      conn = DriverManager.getConnection(connString)
      f(conn)
    } finally {
      if (conn != null) conn.close()
    }
  }

  def one[T](query: String)(implicit m: RowMapper[T]): Option[T] = withConnection { conn ⇒
    var stmt: Statement = null
    var rs: ResultSet = null
    try {
      stmt = conn.createStatement()
      rs = stmt.executeQuery(query)
      singleResult[T](rs)
    } finally {
      if (rs != null) rs.close()
      if (stmt != null) stmt.close()
    }
  }

  def list[T](query: String)(implicit m: RowMapper[T]): List[T] = withConnection { conn ⇒
    var stmt: Statement = null
    var rs: ResultSet = null
    try {
      stmt = conn.createStatement()
      rs = stmt.executeQuery(query)
      (Stream.continually(singleResult[T](rs)) takeWhile (_.nonEmpty)).flatten.toList
    } finally {
      if (rs != null) rs.close()
      if (stmt != null) stmt.close()
    }
  }

  private def singleResult[T](rs: ResultSet)(implicit m: RowMapper[T]): Option[T] =
    m.fromResultSet(rs)
}
