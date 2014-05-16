import java.sql.DriverManager
import scala.collection.mutable

object Main extends App {

  Class.forName("org.h2.Driver")
  val conn = DriverManager.getConnection("jdbc:h2:mem:;MODE=MYSQL")

  val stmt = conn.createStatement()
  stmt.execute("create table foo (a int, b varchar(255))")
  stmt.execute("insert into foo (a, b) values (1, 'a'), (2, 'b')")

  val rs1 = stmt.executeQuery("select b from foo")
  println(Sql.seq[String](rs1).toList)

  val rs2 = stmt.executeQuery("select a from foo")
  println(Sql.one[Int](rs2))

//  val rs3 = stmt.executeQuery("select a, b from foo")
//  println(Sql.seq[(Int, String)](rs3).toList)

  case class Foo(a: Int, b: String)
  val rs4 = stmt.executeQuery("select a, b from foo")
  println(Sql.seq[Foo](rs4).toList)

  case class Bar(b: String)
  val rs5 = stmt.executeQuery("select a, b from foo")
  println(Sql.seq[Bar](rs5).toList)

//  while (rs.next()) {
//    val a = rs.getInt("a")
//    val b = rs.getString("b")
//    println(Foo(a, b))
//  }

  stmt.close()
  conn.close()
}
