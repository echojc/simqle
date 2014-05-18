import java.sql.DriverManager
import scala.collection.mutable

import shapeless._


object A {

  trait Extractor[T] { def extract: T }
  implicit object intExtractor extends Extractor[Int] { def extract = 1 }
  implicit object stringExtractor extends Extractor[String] { def extract = "a" }

  //trait NE[T] { def extract(i: Int): T }
  //implicit object intNExtractor extends NE[Int] { def extract(i: Int) = i }
  //implicit object stringNExtractor extends NE[String] { def extract(i: Int) = ('a' + i - 1).toString }

  implicit object hnilExtractor extends Extractor[HNil] { def extract = HNil }
  trait HListExtractor[L <: HList] extends Extractor[L] {
    type H
    type T <: HList
  }
  object HListExtractor {
    def apply[L <: HList](implicit hlx: HListExtractor[L]): Aux[L, hlx.H, hlx.T] = hlx
    type Aux[L <: HList, H0, T0 <: HList] = HListExtractor[L] { type H = H0; type T = T0 }
    implicit def auxExtractor[H0, T0 <: HList](implicit hx: Extractor[H0], tx: Extractor[T0]): Aux[H0 :: T0, H0, T0] =
      new HListExtractor[H0 :: T0] {
        type H = H0
        type T = T0
        def extract: H0 :: T0 = hx.extract :: tx.extract
      }
  }

  import ops.hlist.Tupler
  implicit def tupleExtractor[P, L <: HList](
    implicit gen: Generic.Aux[P, L], hx: HListExtractor[L], tp: Tupler.Aux[L, P]) =
    new Extractor[P] {
      def extract: P = tp(hx.extract)
    }
}


object Main extends App {
  import A._
  import A.HListExtractor._
  assert(implicitly[Extractor[Int :: String :: HNil]].extract == 1 :: "a" :: HNil)
  assert(implicitly[Extractor[(Int, String)]].extract == (1, "a"))
  //implicitly[Q[(Int, String)]]

//  Class.forName("org.h2.Driver")
//  val conn = DriverManager.getConnection("jdbc:h2:mem:;MODE=MYSQL")
//
//  val stmt = conn.createStatement()
//  stmt.execute("create table foo (a int, b varchar(255))")
//  stmt.execute("insert into foo (a, b) values (1, 'a'), (2, 'b')")
//
//  val rs1 = stmt.executeQuery("select b from foo")
//  println(Sql.seq[String](rs1).toList)
//
//  val rs2 = stmt.executeQuery("select a from foo")
//  println(Sql.one[Int](rs2))
//
////  val rs3 = stmt.executeQuery("select a, b from foo")
////  println(Sql.seq[(Int, String)](rs3).toList)
//
//  case class Foo(a: Int, b: String)
//  val rs4 = stmt.executeQuery("select * from foo")
//  println(Sql.seq[Foo](rs4).toList)
//
//  case class Bar(b: String)
//  val rs5 = stmt.executeQuery("select * from foo")
//  println(Sql.seq[Bar](rs5).toList)
//
////  while (rs.next()) {
////    val a = rs.getInt("a")
////    val b = rs.getString("b")
////    println(Foo(a, b))
////  }
//
//  stmt.close()
//  conn.close()
}
