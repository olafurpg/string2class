package metaconfig.internal

import scala.collection.compat._
import scala.reflect.ClassTag
import metaconfig.Conf
import metaconfig.ConfDecoder
import metaconfig.ConfError
import metaconfig.Configured
import metaconfig.Configured.NotOk
import metaconfig.Configured.Ok

object CanBuildFromDecoder {

  def map[A](
      implicit ev: ConfDecoder[A],
      classTag: ClassTag[A]
  ): ConfDecoder[Map[String, A]] =
    ConfDecoder.fromPartial(
      s"Map[String, ${classTag.runtimeClass.getName}]"
    ) {
      case Conf.Obj(values) =>
        val results = values.map {
          case (key, value) => ev.read(value).map(key -> _)
        }
        ConfError.fromResults(results) match {
          case Some(err) => NotOk(err)
          case None =>
            Ok(results.collect { case Ok(x) => x }.toMap)
        }
    }

  def list[C[_], A](
      implicit ev: ConfDecoder[A],
      factory: Factory[A, C[A]],
      classTag: ClassTag[A]
  ): ConfDecoder[C[A]] =
    ConfDecoder.fromPartial(
      s"List[${classTag.runtimeClass.getName}]"
    ) {
      case Conf.Lst(values) =>
        build(values, ev, factory)(identity, (_, x) => x)
    }

  private def build[A, B, C, Coll](
      values: List[A],
      ev: ConfDecoder[B],
      factory: Factory[C, Coll]
  )(a2conf: A => Conf, ab2c: (A, B) => C): Configured[Coll] = {
    val successB = factory.newBuilder
    val errorB = List.newBuilder[ConfError]
    successB.sizeHint(values.length)
    values.foreach { value =>
      ev.read(a2conf(value)) match {
        case NotOk(e) => errorB += e
        case Ok(decoded) => successB += ab2c(value, decoded)
      }
    }
    ConfError(errorB.result()) match {
      case Some(x) => NotOk(x)
      case None => Ok(successB.result())
    }
  }

}
