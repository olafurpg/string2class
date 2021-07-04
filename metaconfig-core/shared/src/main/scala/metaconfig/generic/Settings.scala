package metaconfig.generic

import metaconfig.Conf
import metaconfig.ConfEncoder
import metaconfig.annotation.DeprecatedName
import metaconfig.internal.Cli
import scala.annotation.StaticAnnotation
import scala.collection.mutable
import metaconfig.annotation.DescriptionDoc
import org.typelevel.paiges.Doc
import metaconfig.annotation.Description
import metaconfig.annotation.Usage
import metaconfig.annotation.ExampleUsage

final class Settings[T](
    val settings: List[Setting],
    val annotations: List[StaticAnnotation]
) {
  def this(settings: List[Setting]) = this(settings, Nil)
  def fields: List[Field] = settings.map(_.field)

  @deprecated("Use flat(Conf) instead", "0.8.0")
  def flat(default: T)(implicit ev: T <:< Product): List[(Setting, Any)] = {
    settings
      .zip(default.productIterator.toIterable)
      .flatMap {
        case (deepSetting, defaultSetting: Product) =>
          deepSetting.flat.zip(defaultSetting.productIterator.toIterable)
        case (s, defaultValue) =>
          (s, defaultValue) :: Nil
      }
  }

  def flat(default: Conf.Obj): List[(Setting, Conf)] = {
    settings.zip(default.values).flatMap {
      case (deepSetting, (_, conf: Conf.Obj)) =>
        deepSetting.underlying.toList
          .flatMap(_.withPrefix(deepSetting.name).flat(conf))
      case (s, (_, defaultValue)) =>
        (s, defaultValue) :: Nil
    }
  }

  def withPrefix(prefix: String): Settings[T] =
    new Settings(settings.map(s => s.withName(prefix + "." + s.name)))

  override def toString: String = s"Surface(settings=$settings)"
  object Deprecated {
    def unapply(key: String): Option[DeprecatedName] =
      (for {
        setting <- settings
        deprecation <- setting.deprecation(key).toList
      } yield deprecation).headOption
  }
  def names: List[String] = settings.map(_.name)
  def nonHiddenNames: List[String] = settings.flatMap { x =>
    if (x.isHidden) None else Some(x.name)
  }
  def allNames: List[String] =
    for {
      setting <- settings
      name <- setting.allNames
    } yield name

  def get(name: String): Option[Setting] =
    settings.find(_.matchesLowercase(name))

  def get(name: String, rest: List[String]): Option[Setting] =
    get(name).flatMap { setting =>
      if (setting.isDynamic) {
        Some(setting)
      } else {
        rest match {
          case Nil => Some(setting)
          case head :: tail =>
            for {
              underlying <- setting.underlying
              next <- underlying.get(head, tail)
            } yield next
        }
      }
    }
  def unsafeGet(name: String): Setting = get(name).get
  @deprecated("Use ConfEncoder[T].write instead", "0.8.1")
  def withDefault(
      default: T
  )(implicit ev: T <:< Product): List[(Setting, Any)] =
    settings.zip(default.productIterator.toList)

  def toCliHelp(default: T)(implicit ev: ConfEncoder[T]): String =
    toCliHelp(default, 80)
  def toCliHelp(default: T, width: Int)(implicit ev: ConfEncoder[T]): String =
    Cli.help[T](default)(ev, this).renderTrim(width)

  def cliDescription: Option[Doc] = annotations.collectFirst {
    case DescriptionDoc(doc) => doc
    case Description(doc) => Doc.text(doc)
  }
  def cliUsage: Option[Doc] = annotations.collectFirst {
    case Usage(doc) => Doc.text(doc)
  }
  def cliExamples: List[Doc] = annotations.collect {
    case ExampleUsage(example) => Doc.text(example)
  }

}

object Settings {
  implicit def FieldsToSettings[T](implicit ev: Surface[T]): Settings[T] =
    apply(ev)
  def apply[T](implicit ev: Surface[T]): Settings[T] = {
    val settings = ev.fields.flatten.map(new Setting(_))
    val errors = validate(settings)
    if (errors.nonEmpty)
      throw new IllegalArgumentException(
        errors.mkString("Can't validate settings:\n", "\n", "")
      )
    new Settings[T](settings, ev.annotations)
  }

  def validate(settings: List[Setting]): Seq[String] = {
    val map = new mutable.HashMap[String, Setting]
    val res = Seq.newBuilder[String]
    settings.foreach { x =>
      val y = map.getOrElseUpdate(x.name, x)
      if (y ne x) res += s"Multiple fields with name: '${x.name}'"
    }
    settings.foreach { x =>
      x.extraNames.foreach { name =>
        val y = map.getOrElseUpdate(name, x)
        if (y ne x)
          res += s"Extra name ($name) for '${x.name}' conflicts '${y.name}'"
      }
    }
    settings.foreach { x =>
      x.deprecatedNames.foreach { dn =>
        val y = map.getOrElseUpdate(dn.name, x)
        if (y ne x)
          res += s"Deprecated name (${dn.name}) for '${x.name}' conflicts '${y.name}'"
      }
    }
    res.result()
  }

  @inline def validate(settings: Settings[_]): Seq[String] =
    validate(settings.settings)

}
