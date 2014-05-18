package sh.echo.simqle

import java.sql.ResultSet
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

import shapeless._

trait RowMapper[T] {
  def fromResultSet(rs: ResultSet): Option[T]
}

object RowMapper extends RowMapperLowerPriorityImplicits {

  import nat._0
  import ops.nat.ToInt
  trait HListRowMapper[L <: HList, N <: Nat] extends RowMapper[L] {
    type H
    type T <: HList
  }
  implicit def hnilRowMapper[N <: Nat] = new HListRowMapper[HNil, N] {
    def fromResultSet(rs: ResultSet): Option[HNil] = Some(HNil)
  }
  object HListRowMapper {
    def apply[L <: HList, N <: Nat](implicit hrm: HListRowMapper[L, N]): Aux[L, hrm.H, hrm.T, N] = hrm
    type Aux[L <: HList, H0, T0 <: HList, N <: Nat] = HListRowMapper[L, N] { type H = H0; type T = T0 }
    implicit def hlistRowMapper[H0, T0 <: HList, N <: Nat](
      implicit hx: ColumnMapper[H0], tx: HListRowMapper[T0, Succ[N]], toInt: ToInt[N]): Aux[H0 :: T0, H0, T0, N] =
      new HListRowMapper[H0 :: T0, N] {
        type H = H0
        type T = T0
        def fromResultSet(rs: ResultSet): Option[H0 :: T0] = {
          val index = toInt() + 1 // ResultSet uses 1-based indexes
          if (index == 1 && !rs.next()) None
          else tx.fromResultSet(rs) map (hx.fromResultSet(rs, index) :: _)
        }
      }
  }

  // we'll do this for now...
  // i don't understand implicit resolution well enough to make this coexist with the tuple one
  object HListSupport {
    implicit def hlistRowMapper[L <: HList](implicit hx: HListRowMapper[L, _0]) = hx
  }

  import ops.hlist.Tupler
  implicit def tupleRowMapper[P, L <: HList](
    implicit gen: Generic.Aux[P, L], hx: HListRowMapper[L, _0], tp: Tupler.Aux[L, P]) =
    new RowMapper[P] {
      def fromResultSet(rs: ResultSet): Option[P] = hx.fromResultSet(rs) map (tp.apply)
    }

  implicit object IntRowMapper extends RowMapper[Int] {
    val columnMapper = implicitly[ColumnMapper[Int]]
    def fromResultSet(rs: ResultSet): Option[Int] = {
      if (!rs.next()) None
      else Option(columnMapper.fromResultSet(rs, 1))
    }
  }

  implicit object StringRowMapper extends RowMapper[String] {
    val columnMapper = implicitly[ColumnMapper[String]]
    def fromResultSet(rs: ResultSet): Option[String] = {
      if (!rs.next()) None
      else Option(columnMapper.fromResultSet(rs, 1))
    }
  }
}

trait RowMapperLowerPriorityImplicits {
  import RowMapperLowerPriorityImplicits._
  implicit def caseClassRowMapper[T]: RowMapper[T] = macro materializeCaseClassRowMapper[T]
}

object RowMapperLowerPriorityImplicits {

  def materializeCaseClassRowMapper[T: c.WeakTypeTag](c: Context): c.Expr[RowMapper[T]] = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val companion = tpe.typeSymbol.companion

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor ⇒ m
    }.get.paramLists.head

    val mappedColumns = fields map { field ⇒
      val name = field.name
      val decoded = name.decodedName.toString
      val fieldTpe = tpe.decl(name).typeSignature
      q"implicitly[ColumnMapper[$fieldTpe]].fromResultSet(rs, $decoded)"
    }

    c.Expr[RowMapper[T]] { q"""
      new RowMapper[$tpe] {
        def fromResultSet(rs: java.sql.ResultSet): Option[$tpe] = {
          if (!rs.next()) None
          else Option($companion(..$mappedColumns))
        }
      }
    """ }
  }
}
