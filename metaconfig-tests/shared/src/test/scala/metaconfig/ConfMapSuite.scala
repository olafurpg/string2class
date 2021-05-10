package metaconfig

object ConfMapSuite {
  case class Foo(m: Map[String, Int] = Map("a" -> 1))

  implicit val surface: generic.Surface[Foo] =
    generic.deriveSurface

  implicit val reader: ConfDecoder[Foo] =
    generic.deriveDecoder[Foo](Foo()).noTypos

}

class ConfMapSuite extends munit.FunSuite {
  import Conf._
  import ConfMapSuite.Foo

  test("simple") {
    val conf = Obj("m" -> Obj("b" -> Num(2)))
    val obtained = conf.as[Foo].get
    val expected = Foo(Map("b" -> 2))
    assertEquals(obtained, expected)
  }

  test("add defaults to map") {
    val conf = Obj("m" -> Obj("add" -> Obj("b" -> Num(2))))
    val obtained = conf.as[Foo].get
    val expected = Foo(Map("a" -> 1, "b" -> 2))
    assertEquals(obtained, expected)
  }
}
