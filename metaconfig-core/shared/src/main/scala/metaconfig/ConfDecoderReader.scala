package metaconfig

import scala.language.higherKinds
import metaconfig.ConfDecoderReader.ConfDecoderFactory
import metaconfig.generic.Settings
import metaconfig.internal.CanBuildFromDecoder

import scala.collection.compat.Factory
import scala.reflect.ClassTag

trait WithDefault[A] { self =>
  def default: A

  final def map[B](f: A => B): WithDefault[B] = new WithDefault[B] {
    override def default: B = f(self.default)
  }
}

object WithDefault {
  def of[A](a: A): WithDefault[A] = new WithDefault[A] {
    override def default: A = a
  }
}

trait ConfDecoderReader[S, A] { self =>
  def decoder: ConfDecoderFactory[S, A]

  def local[S1](f: S1 => S): ConfDecoderReader[S1, A] =
    new ConfDecoderReader[S1, A] {
      override def decoder: ConfDecoderFactory[S1, A] = f.andThen(self.decoder)
    }

  def map[B](f: A => B): ConfDecoderReader[S, B] =
    new ConfDecoderReader[S, B] {
      def decoder = self.decoder.andThen(_.map(f))
    }

  def orElse(other: ConfDecoder[A]): ConfDecoderReader[S, A] =
    new ConfDecoderReader[S, A] {
      def decoder =
        self.decoder.andThen(self => ConfDecoder.orElse(self, other))
    }

  def mapConfigured[TT](f: A => Configured[TT]): ConfDecoderReader[S, TT] =
    new ConfDecoderReader[S, TT] {
      def decoder = self.decoder.andThen(_.flatMap(f))
    }

  def noTypos(implicit ev: Settings[A]): ConfDecoderReader[S, A] =
    new ConfDecoderReader[S, A] {
      def decoder = self.decoder.andThen(_.noTypos)
    }
}

object ConfDecoderReader {

  type ConfDecoderFactory[S, A] = S => ConfDecoder[A]

  implicit def canBuildFromConfDecoderReader[C[X] <: Iterable[X], A, S <: WithDefault[
    C[A]
  ]](
      implicit ev: ConfDecoder[A],
      factory: Factory[A, C[A]],
      classTag: ClassTag[A]
  ): ConfDecoderReader[S, C[A]] =
    CanBuildFromDecoder.listReader

  implicit def mapConfDecoderReader[A, S <: WithDefault[Map[String, A]]](
      implicit ev: ConfDecoder[A],
      classTag: ClassTag[A]
  ): ConfDecoderReader[S, Map[String, A]] = CanBuildFromDecoder.mapReader
}
