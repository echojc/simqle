import java.sql.DriverManager
import scala.collection.mutable

object Main extends App {

  Class.forName("org.h2.Driver")
  val conn = DriverManager.getConnection("jdbc:h2:mem:;MODE=MYSQL")

  val stmt = conn.createStatement()
  stmt.execute("create table foo (a int, b varchar(255))")
  stmt.execute("insert into foo (a, b) values (1, 'a'), (2, 'b')")

  val rs1 = stmt.executeQuery("select b from foo")
  assert(Sql.list[String](rs1) == List("a", "b"))

  val rs2 = stmt.executeQuery("select a from foo")
  assert(Sql.one[Int](rs2) == Option(1))

  val rs3 = stmt.executeQuery("select a, b from foo")
  assert(Sql.list[(Int, String)](rs3) == List((1, "a"), (2, "b")))

  case class Foo(a: Int, b: String)
  val rs4a = stmt.executeQuery("select * from foo where b = 'b'")
  assert(Sql.one[Foo](rs4a) == Option(Foo(2, "b")))
  val rs4b = stmt.executeQuery("select * from foo where b = 'zzz'")
  assert(Sql.one[Foo](rs4b) == None)

  case class Bar(b: String)
  val rs5 = stmt.executeQuery("select * from foo")
  assert(Sql.list[Bar](rs5) == List(Bar("a"), Bar("b")))

  stmt.close()
  conn.close()
}
