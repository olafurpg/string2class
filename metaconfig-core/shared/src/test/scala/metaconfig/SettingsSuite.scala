package metaconfig

import metaconfig.annotation.Deprecated
import metaconfig.annotation.DeprecatedName
import metaconfig.annotation.ExtraName
import metaconfig.generic.Settings

class SettingsSuite extends munit.FunSuite {

  case class ToString(@ExtraName("extra") name: String)
  test(".toString") {
    implicit val surface = generic.deriveSurface[ToString]
    val obtained = Settings[ToString].toString
    val expected =
      """Surface(settings=List(Setting(Field(name="name",tpe="String",annotations=List(@ExtraName(extra)),underlying=List()))))"""
    assertEquals(obtained, expected)
  }

  val List(s1, s2, _) = Settings[AllTheAnnotations].settings

  test("name") {
    assertEquals(s1.name, "number")
  }

  test("extraNames") {
    assertEquals(
      s1.extraNames,
      List(
        "extraName",
        "extraName2"
      )
    )
  }

  test("deprecatedNames") {
    assertEquals(
      s1.deprecatedNames,
      List(
        DeprecatedName("deprecatedName", "Use x instead", "2.0"),
        DeprecatedName("deprecatedName2", "Use y instead", "3.0")
      )
    )
  }

  test("exampleValues") {
    assertEquals(
      s1.exampleValues,
      List("value", "value2")
    )
  }

  test("description") {
    assert(clue(s1.description).contains("descriptioon"))
  }

  test("sinceVersion") {
    assert(s1.sinceVersion.contains("2.1"))
  }

  test("deprecated") {
    assert(
      s1.deprecated.contains(Deprecated("Use newFeature instead", "2.1"))
    )
  }

  test("annotations") {
    assert(s2.annotations.isEmpty)
  }

  test("flat") {
    val flat = Settings[Nested]
      .flat(ConfEncoder[Nested].writeObj(Nested()))
      .map { case (s, c) => s"${s.name} $c" }
      .mkString("\n")
    assertNoDiff(
      flat,
      """a 31
        |b.param 82
        |c.c "nested2"
        |c.b.param 82""".stripMargin
    )
  }
}
