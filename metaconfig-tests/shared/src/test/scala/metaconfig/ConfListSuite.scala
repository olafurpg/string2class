package metaconfig

object ConfListSuite {

  case class Bar(add: List[String] = Nil)

  implicit val barSurface: generic.Surface[Bar] =
    generic.deriveSurface

  implicit val barReader: ConfDecoder[Bar] =
    generic.deriveDecoder[Bar](Bar()).noTypos

  case class Foo(
      field: List[String] = List("c"),
      anotherField: List[String] = Nil,
      bar: Bar = Bar()
  )
  implicit val surface: generic.Surface[Foo] =
    generic.deriveSurface

  implicit val reader: ConfDecoder[Foo] =
    generic.deriveDecoder[Foo](Foo()).noTypos

  case class Nested(raw: String = "x")
  implicit val surfaceNested: generic.Surface[Nested] =
    generic.deriveSurface

  implicit val readerNested: ConfDecoder[Nested] =
    generic
      .deriveDecoder[Nested](Nested())
      .map(x => Nested(x.raw + "_mapped"))
      .noTypos

  case class Baz(as: List[Nested] = List(Nested("a")))

  implicit val surfaceBaz: generic.Surface[Baz] =
    generic.deriveSurface

  implicit val readerBaz: ConfDecoder[Baz] =
    generic.deriveDecoder[Baz](Baz()).noTypos

  case class FromString(str: String)

  implicit val fromStringListReader
      : ConfDecoderReader[WithDefault[List[FromString]], List[FromString]] =
    implicitly[ConfDecoderReader[WithDefault[List[String]], List[String]]]
      .map(x => x.map(a => FromString(a + "_mapped")))
      .local(_.map(_.map(_.str)))

  case class Caz(as: List[FromString] = List(FromString("y")))

  implicit val surfaceCaz: generic.Surface[Caz] =
    generic.deriveSurface

  implicit val readerCaz: ConfDecoder[Caz] =
    generic.deriveDecoder[Caz](Caz()).noTypos

  case class OneParam(param: Int = 82)
  object OneParam {
    implicit val surface: generic.Surface[OneParam] = generic.deriveSurface
    implicit val decoder: ConfDecoder[OneParam] =
      generic.deriveDecoder[OneParam](OneParam())
  }
  case class Nested2(c: String = "nested2", b: Set[OneParam] = Set(OneParam(1)))
  object Nested2 {
    implicit val surface: generic.Surface[Nested2] =
      generic.deriveSurface
    implicit val decoder: ConfDecoder[Nested2] =
      generic.deriveDecoder[Nested2](Nested2())
  }

  case class MultiNested(
      a: Int = 31,
      b: OneParam = OneParam(),
      c: Nested2 = Nested2()
  )
  object MultiNested {
    implicit val surface: generic.Surface[MultiNested] =
      generic.deriveSurface
    implicit val decoder: ConfDecoder[MultiNested] =
      generic.deriveDecoder[MultiNested](MultiNested())
  }
}

class ConfListSuite extends munit.FunSuite {
  import Conf._
  import ConfListSuite._

  test("simple") {
    val conf = Obj("field" -> Lst(Str("a"), Str("b")))
    val obtained = conf.as[Foo].get
    val expected = Foo(List("a", "b"))
    assertEquals(obtained, expected)
  }

  test("missing") {
    val conf = Obj()
    val obtained = conf.as[Foo].get
    val expected = Foo(List("c"), Nil)
    assertEquals(obtained, expected)
  }

  test("add to default") {
    val conf = Obj("field" -> Obj("add" -> Lst(Str("a"), Str("b"))))
    val obtained = conf.as[Foo].get
    val expected = Foo(List("c", "a", "b"))
    assertEquals(obtained, expected)
  }

  test("don't touch another fields with same type") {
    val conf = Obj(
      "field" -> Obj("add" -> Lst(Str("a"), Str("b"))),
      "anotherField" -> Lst(Str("d"))
    )
    val obtained = conf.as[Foo].get
    val expected = Foo(List("c", "a", "b"), List("d"))
    assertEquals(obtained, expected)
  }

  test("read config from foo.bar.add") {
    val conf = Obj(
      "field" -> Obj("add" -> Lst(Str("a"), Str("b"))),
      "anotherField" -> Lst(Str("d")),
      "bar" -> Obj("add" -> Lst(Str("e")))
    )
    val obtained = conf.as[Foo].get
    val expected = Foo(List("c", "a", "b"), List("d"), Bar(List("e")))
    assertEquals(obtained, expected)
  }

  test("read nested records") {
    val conf = Obj("as" -> Obj("add" -> Lst(Obj("raw" -> Str("b")))))
    val obtained = conf.as[Baz].get
    val expected = Baz(List(Nested("a"), Nested("b_mapped")))
    assertEquals(obtained, expected)
  }

  test("read nested records from primitives") {
    val conf = Obj("as" -> Obj("add" -> Lst(Str("b"))))
    val obtained = conf.as[Caz].get
    val expected = Caz(List(FromString("y_mapped"), FromString("b_mapped")))
    assertEquals(obtained, expected)
  }

  test("read multi nested records") {
    val conf = Obj("c" -> Obj("b" -> Obj("add" -> Lst(Obj("param" -> Num(2))))))
    val obtained = conf.as[MultiNested].get
    val expected = MultiNested(
      31,
      OneParam(),
      Nested2("nested2", Set(OneParam(1), OneParam(2)))
    )
    assertEquals(obtained, expected)
  }
}
