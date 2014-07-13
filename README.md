# simqle


An experiment with automatic conversion between SQL queries and Scala types
via the typeclass pattern.

[See the presentation here](https://echojc.github.io/simqle).

### Examples

```scala
val db = Db("jdbc:h2:mem:demo;MODE=MYSQL;DB_CLOSE_DELAY=-1")

db.withConnection { conn ⇒
  val stmt = conn.createStatement()
  stmt.execute("create table foo (a int, b varchar(255))")
  stmt.execute("insert into foo (a, b) values (1, 'a'), (2, 'b')")
  stmt.close()
}

assert(db.list[String]("select b from foo") == List("a", "b"))

assert(db.one[Int]("select a from foo") == Option(1))

assert(db.list[(Int, String)]("select a, b from foo") == List((1, "a"), (2, "b")))

case class Foo(a: Int, b: String)
assert(db.one[Foo]("select * from foo where b = 'b'") == Option(Foo(2, "b")))
assert(db.one[Foo]("select * from foo where b = 'z'") == None)

case class Bar(b: String)
assert(db.list[Bar]("select * from foo") == List(Bar("a"), Bar("b")))
```

You can also map to HLists directly:

```scala
import shapeless._
import RowMapper.HListSupport._
assert(db.list[Int :: String :: HNil]("select a, b from foo") == List(1 :: "a" :: HNil, 2 :: "b" :: HNil))
```
