package metaconfig

import org.scalatest.FunSuite

class ConfigReaderTest extends FunSuite {
  type Result[T] = Either[Throwable, T]

  @ConfigReader
  case class Inner(nest: Int)

  @ConfigReader
  case class Outer(i: Int, inner: Inner) {
    implicit val innerReader: Reader[Inner] = inner.reader
  }

  @ConfigReader
  case class Bar(i: Int, b: Boolean, s: String)

  @ConfigReader
  case class HasList(i: Seq[Int])

  @ConfigReader
  case class HasMap(i: Map[String, Int])

  val b = Bar(0, true, "str")
  test("invalid field") {
    assert(
      b.reader.read(Conf.Obj("is" -> Conf.Num(2), "var" -> Conf.Num(3))) ==
        Left(
          ConfigError("Error reading class 'Bar'. Invalid fields: is, var")))
  }

  test("read OK") {
    assert(
      b.reader.read(Conf.Obj("i" -> Conf.Num(2))) ==
        Right(b.copy(i = 2)))
    assert(
      b.reader.read(Conf.Obj("s" -> Conf.Str("str"))) ==
        Right(b.copy(s = "str")))
    assert(
      b.reader.read(Conf.Obj("b" -> Conf.Bool(true))) ==
        Right(b.copy(b = true)))
    assert(
      b.reader.read(
        Conf.Obj(
          "i" -> Conf.Num(3),
          "b" -> Conf.Bool(true),
          "s" -> Conf.Str("rand")
        )) == Right(b.copy(i = 3, s = "rand", b = true)))
  }
  test("unexpected type") {
    val msg =
      "Error reading field 'i'. Expected argument of type int. Obtained value 'str' of type String."
    val Left(e @ FailedToReadClass("Bar", _)) =
      b.reader.read(Conf.Obj("i" -> Conf.Str("str")))
    assert(e.getMessage.endsWith(msg))
  }

  test("write OK") {
    assert(
      b.fields == Map(
        "i" -> 0,
        "b" -> true,
        "s" -> "str"
      ))
  }
  test("nested OK") {
    val m = Conf.Obj(
      "i" -> Conf.Num(4),
      "inner" -> Conf.Obj(
        "nest" -> Conf.Num(5)
      )
    )
    val o = Outer(2, Inner(3)).reader.read(m)
  }

  test("Seq") {
    val lst = HasList(List(1, 2, 3))
    assert(
      lst.reader
        .read(Conf.Obj("i" -> Conf.Lst(Conf.Num(666), Conf.Num(777)))) ==
        Right(HasList(Seq(666, 777))))
  }

  test("Conf.Obj") {
    val lst = HasMap(Map("1" -> 2))
    assert(
      lst.reader.read(Conf.Obj("i" -> Conf.Obj("666" -> Conf.Num(777)))) ==
        Right(HasMap(Map("666" -> 777))))
  }

  case object Kase
  @ConfigReader
  case class Ob(kase: Kase.type) {
    implicit val KaseReader: Reader[Kase.type] = Reader.stringR.flatMap { x =>
      ???
    }
  }

  test("Runtime ???") {
    val m = Conf.Obj(
      "kase" -> Conf.Str("string")
    )
    val Left(e @ FailedToReadClass("Ob", _: NotImplementedError)) =
      Ob(Kase).reader.read(m)
    org.scalameta.logger.elem(e)
    assert(e.getMessage().startsWith("Failed to read 'Ob'"))
  }

  @ConfigReader
  case class HasExtra(@ExtraName("b") @metaconfig.ExtraName("c") a: Int)
  test("@ExtraName") {
    val x = HasExtra(1)
    val Right(HasExtra(2)) = x.reader.read(Conf.Obj("b" -> Conf.Num(2)))
    val Right(HasExtra(3)) = x.reader.read(Conf.Obj("c" -> Conf.Num(3)))
    val Left(_) = x.reader.read(Conf.Obj("d" -> Conf.Num(3)))
  }
}
