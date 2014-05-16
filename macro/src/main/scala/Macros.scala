import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import java.sql.ResultSet
import shapeless._

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

trait RowMapper[T] {
  def fromResultSet(rs: ResultSet): Option[T]
}

object RowMapper {

  implicit def caseClassRowMapper[T]: RowMapper[T] = macro materializeCaseClassRowMapper[T]

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

//  implicit def tuple2RowMapper[A: ColumnMapper, B: ColumnMapper]: RowMapper[(A, B)] =
//    new RowMapper[(A, B)] {
//      val columnMapperA = implicitly[ColumnMapper[A]]
//      val columnMapperB = implicitly[ColumnMapper[B]]
//      def fromResultSet(rs: ResultSet): Option[(A, B)] = {
//        if (!rs.next()) None
//        else Option((
//          columnMapperA.fromResultSet(rs, 1),
//          columnMapperB.fromResultSet(rs, 2)
//        ))
//      }
//    }

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

object Sql {

  def one[T](rs: ResultSet)(implicit m: RowMapper[T]): Option[T] =
    m.fromResultSet(rs)

  def seq[T](rs: ResultSet)(implicit m: RowMapper[T]): Seq[T] =
    (Stream.continually(one(rs)) takeWhile (_.nonEmpty)).flatten
}
