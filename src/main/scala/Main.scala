import java.sql.DriverManager
import scala.collection.mutable

import shapeless._


object A {

  trait Extractor[T] { def extract: T }
  implicit object intExtractor extends Extractor[Int] { def extract = 1 }
  implicit object stringExtractor extends Extractor[String] { def extract = "a" }

  trait NE[T] { def extract(i: Int): T }
  implicit object intNExtractor extends NE[Int] { def extract(i: Int) = i }
  implicit object stringNExtractor extends NE[String] { def extract(i: Int) = ('a' + i - 1).toChar.toString }
  implicit object doubleNExtractor extends NE[Double] { def extract(i: Int) = 1.0 * i }

  import nat._
  import ops.nat._
  trait LowerPriority {
    implicit def hlistExtractor[L <: HList](implicit hlnx: HListNExtractor[L, _2]): HListNExtractor[L, _2] = hlnx
  }

  trait HListNExtractor[L <: HList, N <: Nat] extends Extractor[L] {
    type H
    type T <: HList
  }
  implicit def hnilNExtractor[N <: Nat] = new HListNExtractor[HNil, N] { def extract = HNil }
  object HListNExtractor extends LowerPriority {
    def apply[L <: HList, N <: Nat](implicit hlx: HListNExtractor[L, N]): Aux[L, hlx.H, hlx.T, N] = hlx

    type Aux[L <: HList, H0, T0 <: HList, N <: Nat] = HListNExtractor[L, N] { type H = H0; type T = T0 }

    implicit def auxExtractor[H0, T0 <: HList, N <: Nat](
      implicit hx: NE[H0], tx: HListNExtractor[T0, Succ[N]], toInt: ToInt[N]): Aux[H0 :: T0, H0, T0, N] =
      new HListNExtractor[H0 :: T0, N] {
        type H = H0
        type T = T0
        def extract: H0 :: T0 = hx.extract(toInt()) :: tx.extract
      }
  }

  import ops.hlist.Tupler
  implicit def tupleExtractor[P, L <: HList](
    implicit gen: Generic.Aux[P, L], hx: HListNExtractor[L, _1], tp: Tupler.Aux[L, P]) =
    new Extractor[P] {
      def extract: P = tp(hx.extract)
    }
}


object Main extends App {
  import A._
  import A.HListNExtractor._
  assert(implicitly[Extractor[Int :: String :: HNil]].extract == 1 :: "b" :: HNil)
  assert(implicitly[Extractor[(String, Int)]].extract == ("a", 2))
  assert(implicitly[Extractor[(Int, Double, String, Double, String, Int)]].extract == (1, 2.0, "c", 4.0, "e", 6))

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
