package metaconfig.typesafeconfig

import metaconfig.{Conf, ConfCodec, generic}

object ReadHoconToCaseClassSuite {
  case class Bar(lst: List[String] = List("a"))

  implicit val barSurface: generic.Surface[Bar] =
    generic.deriveSurface

  implicit val barCodec: ConfCodec[Bar] =
    generic.deriveCodec[Bar](Bar())

  case class Foo(
      x: String = "x",
      y: Int = 2,
      bar: Bar = Bar(),
      xs: Map[String, Int] = Map("a" -> 1),
      ys: List[Int] = List(1, 2)
  )

  implicit val surface: generic.Surface[Foo] =
    generic.deriveSurface

  implicit val codec: ConfCodec[Foo] =
    generic.deriveCodec[Foo](Foo())
}

class ReadHoconToCaseClassSuite extends munit.FunSuite {
  import ReadHoconToCaseClassSuite.{Bar, Foo}

  test("read case class simple") {
    val hocon = """
    x = "hello"
    bar.lst = ["b"]
    xs = {
      b = 4
    }
    ys = [3]
    """

    val parsed = Conf.parseString(hocon).get.as[Foo].get
    val expected =
      Foo("hello", 2, Bar(List("b")), Map("b" -> 4), List(3))
    assertEquals(parsed, expected)
  }

  test("read case class from hocon with append") {
    val hocon = """
    x = "hello"
    bar.lst.add = ["b"]
    xs.add = {
      b = 4
    }
    ys = [3]
    """

    val parsed = Conf.parseString(hocon).get.as[Foo].get
    val expected =
      Foo("hello", 2, Bar(List("a", "b")), Map("a" -> 1, "b" -> 4), List(3))
    assertEquals(parsed, expected)
  }

}
