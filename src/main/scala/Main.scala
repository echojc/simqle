import java.sql.DriverManager
import scala.collection.mutable

import sh.echo.simqle._

object Main extends App {

  Class.forName("org.h2.Driver")
  val db = Db("jdbc:h2:mem:demo;MODE=MYSQL;DB_CLOSE_DELAY=-1")

  db.withConnection { conn ⇒
    val stmt = conn.createStatement()
    stmt.execute("create table foo (a int, b varchar(255))")
    stmt.execute("insert into foo (a, b) values (1, 'a'), (2, 'b')")
    stmt.close()
  }

  case class Foo(a: Int, b: String)
  case class Bar(b: String)

  assert(db.list[String]("select b from foo") == List("a", "b"))
  assert(db.one[Int]("select a from foo") == Option(1))
  assert(db.list[(Int, String)]("select a, b from foo") == List((1, "a"), (2, "b")))
  assert(db.one[Foo]("select * from foo where b = 'b'") == Option(Foo(2, "b")))
  assert(db.one[Foo]("select * from foo where b = 'z'") == None)
  assert(db.list[Bar]("select * from foo") == List(Bar("a"), Bar("b")))

  {
    import shapeless._
    import RowMapper.HListSupport._
    assert(db.list[Int :: String :: HNil]("select a, b from foo") == List(1 :: "a" :: HNil, 2 :: "b" :: HNil))
  }
}
