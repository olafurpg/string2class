package metaconfig.internal

import metaconfig.ConfDecoderReader.ConfDecoderFactory
import metaconfig.Configured.{NotOk, Ok}
import metaconfig._

import scala.collection.compat._
import scala.language.higherKinds
import scala.reflect.ClassTag

object CanBuildFromDecoder {
  def mapReader[A, S <: WithDefault[Map[String, A]]](
      implicit ev: ConfDecoder[A],
      classTag: ClassTag[A]
  ): ConfDecoderReader[S, Map[String, A]] =
    new ConfDecoderReader[S, Map[String, A]] {
      override def decoder: ConfDecoderFactory[S, Map[String, A]] =
        state =>
          new ConfDecoder[Map[String, A]] {
            private val underlying = CanBuildFromDecoder.map[A]
            override def read(conf: Conf): Configured[Map[String, A]] =
              conf match {
                case Conf.Obj(("add", conf) :: Nil) =>
                  underlying.read(conf).map { res => state.default ++ res }
                case els => underlying.read(els)
              }
          }
    }

  def map[A](
      implicit ev: ConfDecoder[A],
      classTag: ClassTag[A]
  ): ConfDecoder[Map[String, A]] =
    ConfDecoder.instanceExpect[Map[String, A]](
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

  def listReader[C[X] <: Iterable[X], A, S <: WithDefault[C[A]]](
      implicit ev: ConfDecoder[A],
      factory: Factory[A, C[A]],
      classTag: ClassTag[A]
  ): ConfDecoderReader[S, C[A]] =
    new ConfDecoderReader[S, C[A]] {
      override def decoder: S => ConfDecoder[C[A]] =
        state =>
          new ConfDecoder[C[A]] {
            private val underlying = list[C, A]

            override def read(conf: Conf): Configured[C[A]] =
              conf match {
                case Conf.Obj(("add", conf) :: Nil) =>
                  val builder = factory.newBuilder
                  underlying.read(conf).map { res =>
                    state.default.foreach(builder += _)
                    res.foreach(builder += _)
                    builder.result()
                  }
                case els => underlying.read(els)
              }
          }
    }

  def list[C[_], A](
      implicit ev: ConfDecoder[A],
      factory: Factory[A, C[A]],
      classTag: ClassTag[A]
  ): ConfDecoder[C[A]] =
    new ConfDecoder[C[A]] {
      override def read(conf: Conf): Configured[C[A]] = conf match {
        case Conf.Lst(values) =>
          val successB = factory.newBuilder
          val errorB = List.newBuilder[ConfError]
          successB.sizeHint(values.length)
          values.foreach { value =>
            ev.read(value) match {
              case NotOk(e) => errorB += e
              case Ok(e) => successB += e
            }
          }
          ConfError(errorB.result()) match {
            case Some(x) => NotOk(x)
            case None => Ok(successB.result())
          }
        case _ =>
          val error = ConfError.typeMismatch(
            s"List[${classTag.runtimeClass.getName}]",
            conf
          )
          NotOk(error)
      }
    }
}
