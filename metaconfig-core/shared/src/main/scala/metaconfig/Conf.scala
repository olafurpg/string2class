package metaconfig

import java.io.File
import scala.util.Try
import metaconfig.Extractors._
import metaconfig.ConfDecoder.ConfDecoderWithDefaultMaybe
import metaconfig.generic.Setting
import metaconfig.generic.Settings
import metaconfig.internal.CliParser
import metaconfig.internal.ConfGet
import metaconfig.internal.ConfPatch
import metaconfig.internal.HoconPrinter

sealed abstract class Conf extends Product with Serializable {
  def dynamic: ConfDynamic = ConfDynamic(Configured.Ok(this))
  def pos: Position = Position.None
  final def withPos(pos: Position): Conf = ConfOps.withPos(this, pos)
  final def normalize: Conf = ConfOps.normalize(this)
  final def kind: String = ConfOps.kind(this)
  final def show: String = ConfOps.show(this)
  final def foreach(f: Conf => Unit): Unit = ConfOps.foreach(this)(f)
  @deprecated("No longer supported", "0.7.1")
  final def diff(other: Conf): Option[(Conf, Conf)] = ConfOps.diff(this, other)
  final override def toString: String = show
  def as[T](implicit ev: ConfDecoder[T]): Configured[T] =
    ev.read(this)
  def getSettingOrElse[T](setting: Setting, default: T)(
      implicit ev: ConfDecoderWithDefaultMaybe[T]
  ): Configured[T] =
    ConfGet.getOrElse(this, default, setting.name, setting.alternativeNames: _*)
  def get[T](path: String, extraNames: String*)(
      implicit ev: ConfDecoder[T]
  ): Configured[T] =
    ConfGet.get(this, path, extraNames: _*)
  def getOrElse[T](path: String, extraNames: String*)(
      default: T
  )(implicit ev: ConfDecoderWithDefaultMaybe[T]): Configured[T] =
    ConfGet.getOrElse(this, default, path, extraNames: _*)
}

object Conf {
  def fromMap(map: Map[String, Conf]): Conf = Conf.Obj(map.toList)
  def fromList(lst: List[Conf]): Conf = Conf.Lst(lst)
  def fromBoolean(bool: Boolean): Conf = Conf.Bool(bool)
  def fromInt(n: Int): Conf = Conf.Num(n)
  def fromBigDecimal(n: BigDecimal): Conf = Conf.Num(n)
  def fromNumberOrString(str: String): Conf =
    Try(fromBigDecimal(BigDecimal(str.toDouble))).getOrElse(fromString(str))
  def fromString(str: String): Conf = Conf.Str(str)

  def parseCliArgs[T](
      args: List[String]
  )(implicit settings: Settings[T]): Configured[Conf] =
    CliParser.parseArgs[T](args)
  def parseFile(
      file: File
  )(implicit parser: MetaconfigParser): Configured[Conf] =
    parseInput(Input.File(file))
  def parseString(
      string: String
  )(implicit parser: MetaconfigParser): Configured[Conf] =
    parseInput(Input.String(string))
  def parseString(filename: String, string: String)(
      implicit parser: MetaconfigParser
  ): Configured[Conf] =
    parseInput(Input.VirtualFile(filename, string))
  def parseInput(
      input: Input
  )(implicit parser: MetaconfigParser): Configured[Conf] =
    parser.fromInput(input)

  /** Pretty-print this value as a HOCON string. */
  def printHocon[T: ConfEncoder](value: T): String = {
    HoconPrinter.toHocon(value).renderTrim(100)
  }

  /** Produce a minimal Conf that when merged with original yields revised. **/
  def patch(original: Conf, revised: Conf): Conf =
    ConfPatch.patch(original, revised)

  /** Applies the patch configuration on top of original. */
  def applyPatch(original: Conf, patch: Conf): Conf =
    ConfOps.merge(original, patch)

  case class Null() extends Conf
  case class Str(value: String) extends Conf
  case class Num(value: BigDecimal) extends Conf
  case class Bool(value: Boolean) extends Conf
  case class Lst(values: List[Conf]) extends Conf
  object Lst { def apply(values: Conf*): Lst = Lst(values.toList) }
  case class Obj(values: List[(String, Conf)]) extends Conf {
    override final def equals(obj: scala.Any): Boolean =
      this.eq(obj.asInstanceOf[AnyRef]) || {
        obj match {
          case o: Conf.Obj => map equals o.map // Ignore key ordering.
          case _ => false
        }
      }
    lazy val map: Map[String, Conf] = values.toMap
    def field(key: String): Option[Conf] = map.get(key)
    def keys: List[String] = values.map(_._1)
    def mapValues(f: Conf => Conf): Obj =
      Obj(values.map {
        case (k, v) => k -> f(v)
      })
    def getOption[T](path: String, extraNames: String*)(
        implicit ev: ConfDecoder[T]
    ): Configured[Option[T]] =
      ConfGet
        .getKey(this, path +: extraNames)
        .map(value => ev.read(value).map(Some(_)))
        .getOrElse(Configured.Ok(None))
  }
  object Obj {
    val empty: Obj = Obj()
    def apply(values: (String, Conf)*): Obj = Obj(values.toList)
  }
}

object ConfOps {
  import Conf._
  def withPos(conf: Conf, newPos: Position): Conf = conf match {
    case Conf.Obj(value) =>
      // Subtyping case classes is bad, but I'm too lazy and I couldn't find a less
      // verbose way to include positions. The alternative I could think of was
      // to write the custom apply/unapply/toString OR pattern match on
      // `case Obj(value, pos)`, which I don't want to do either.
      new Conf.Obj(value) { override def pos: Position = newPos }
    case Conf.Lst(value) =>
      new Conf.Lst(value) { override def pos: Position = newPos }
    case Conf.Str(value) =>
      new Conf.Str(value) { override def pos: Position = newPos }
    case Conf.Bool(value) =>
      new Conf.Bool(value) { override def pos: Position = newPos }
    case Conf.Num(value) =>
      new Conf.Num(value) { override def pos: Position = newPos }
    case Conf.Null() =>
      new Conf.Null() { override def pos: Position = newPos }
  }

  def diff(a: Conf, b: Conf): Option[(Conf, Conf)] = (a, b) match {
    case (o1 @ Obj(v1), o2 @ Obj(v2)) =>
      if (o1.keys != o2.keys) Some(a -> b)
      else
        v1.map(_._2)
          .zip(v2.map(_._2))
          .flatMap {
            case (a, b) => diff(a, b)
          }
          .headOption
    case (Lst(l1), Lst(l2)) =>
      if (l1.lengthCompare(l2.length) != 0) Some(a -> b)
      else {
        l1.zip(l1)
          .flatMap {
            case (c1, c2) =>
              diff(c1, c2)
          }
          .headOption
      }
    case (Str(x), Str(y)) => if (x != y) Some(a -> b) else None
    case (Bool(x), Bool(y)) => if (x != y) Some(a -> b) else None
    case (Num(x), Num(y)) => if (x != y) Some(a -> b) else None
    case (Null(), Null()) => None
    case _ => Some(a -> b)
  }

  def sortKeys(c: Conf): Conf =
    ConfOps.fold(c)(obj = x => Conf.Obj(x.values.sortBy(_._1)))

  def foreach(conf: Conf)(f: Conf => Unit): Unit = conf match {
    case Str(_) | Bool(_) | Num(_) | Null() => f(conf)
    case Lst(values) => f(conf); values.foreach(x => foreach(x)(f))
    case Obj(values) => f(conf); values.foreach(x => foreach(x._2)(f))
  }
  def fold(conf: Conf)(
      str: Str => Str = identity,
      num: Num => Num = identity,
      bool: Bool => Bool = identity,
      lst: Lst => Lst = identity,
      obj: Obj => Obj = identity
  ): Conf = conf match {
    case x @ Str(_) => str(x)
    case x @ Bool(_) => bool(x)
    case x @ Num(_) => num(x)
    case Null() => Null()
    case x @ Lst(_) =>
      Lst(lst(x).values.map(y => fold(y)(str, num, bool, lst, obj)))
    case x @ Obj(_) =>
      obj(x).mapValues(y => fold(y)(str, num, bool, lst, obj))
  }

  def escape(str: String): String =
    str.flatMap {
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '"' => "\""
      case other => other.toString
    }

  // TODO(olafur) use something like Paiges to get pretty output.
  final def show(conf: Conf): String = conf match {
    case Str(v) => "\"%s\"".format(escape(v))
    case Num(v) => v.toString()
    case Bool(v) => v.toString
    case Null() => "null"
    case Lst(vs) => vs.map(show).mkString("[", ", ", "]")
    case Obj(vs) =>
      vs.map { case (a, b) => s""""$a": ${show(b)}""" }
        .mkString("{", ", ", "}")
  }

  final def normalize(conf: Conf): Conf = {
    def expandKeys(conf: Conf): Conf = conf match {
      case Conf.Num(_) => conf
      case Conf.Bool(_) => conf
      case Conf.Str(_) => conf
      case Conf.Null() => conf
      case Conf.Lst(values) => Conf.Lst(values.map(normalize))
      case Conf.Obj(values) =>
        val expandedKeys = values.map {
          case (NestedKey(key, rest), value) =>
            key -> normalize(Obj(rest -> value))
          case (key, value) =>
            key -> normalize(value)
        }
        Obj(expandedKeys)
    }
    def mergeKeys(conf: Conf): Conf = conf match {
      case x @ Obj(_) => merge(Obj.empty, x)
      case x => x
    }
    sortKeys(mergeKeys(expandKeys(conf)))
  }

  final def merge(a: Conf, b: Conf): Conf = (a, b) match {
    case (Obj(v1), Obj(v2)) =>
      val merged = (v1 ++ v2).foldLeft(Vector.empty[(String, Conf)]) {
        case (accumulated, pair @ (key, value2)) =>
          accumulated
            .collectFirst {
              case (`key`, value1) =>
                accumulated.filter(_._1 != key) ++
                  Vector((key, merge(value1, value2)))
            }
            .getOrElse(pair +: accumulated)
      }
      Obj(merged.toList)
    case (_, _) => b
  }

  final def kind(conf: Conf): String = conf match {
    case Str(_) => "String"
    case Num(_) => "Number"
    case Bool(_) => "Boolean"
    case Lst(_) => "List[T]"
    case Obj(_) => "Map[K, V]"
    case Null() => "Null"
  }
}
