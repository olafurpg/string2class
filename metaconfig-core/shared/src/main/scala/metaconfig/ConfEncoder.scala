package metaconfig

import scala.language.higherKinds
import java.nio.file.Path

trait ConfEncoder[A] { self =>
  def write(value: A): Conf
  final def writeObj(value: A): Conf.Obj =
    write(value) match {
      case o: Conf.Obj => o
      case els => ConfError.typeMismatch("Conf.Obj", els).notOk.get
    }

  final def contramap[B](f: B => A): ConfEncoder[B] = new ConfEncoder[B] {
    override def write(value: B): Conf = self.write(f(value))
  }
}

object ConfEncoder {
  def empty[A]: ConfEncoder[A] = instance[A](_ => Conf.Null())

  def apply[A](implicit ev: ConfEncoder[A]): ConfEncoder[A] = ev

  def instance[A](f: A => Conf): ConfEncoder[A] = new ConfEncoder[A] {
    override def write(value: A): Conf = f(value)
  }

  private val GenericConfEncoder: ConfEncoder[Conf] =
    new ConfEncoder[Conf] {
      override def write(value: Conf): Conf = value
    }

  // Invariant type-classes don't work unfortunately so we do this work by hand.
  implicit def ConfEncoder[T <: Conf]: ConfEncoder[T] =
    GenericConfEncoder.asInstanceOf[ConfEncoder[T]]

  implicit val BooleanEncoder: ConfEncoder[Boolean] =
    new ConfEncoder[Boolean] {
      override def write(value: Boolean): Conf = Conf.Bool(value)
    }

  implicit val IntEncoder: ConfEncoder[Int] =
    new ConfEncoder[Int] {
      override def write(value: Int): Conf = Conf.Num(value)
    }

  implicit val UnitEncoder: ConfEncoder[Unit] =
    new ConfEncoder[Unit] {
      override def write(value: Unit): Conf = Conf.Null()
    }

  implicit val StringEncoder: ConfEncoder[String] =
    new ConfEncoder[String] {
      override def write(value: String): Conf = Conf.Str(value)
    }

  implicit lazy val PathEncoder: ConfEncoder[Path] =
    new ConfEncoder[Path] {
      override def write(value: Path): Conf = Conf.Str(value.toString())
    }

  implicit def IterableEncoder[A, C[x] <: Iterable[x]](
      implicit ev: ConfEncoder[A]
  ): ConfEncoder[C[A]] =
    new ConfEncoder[C[A]] {
      override def write(value: C[A]): Conf = {
        Conf.Lst(value.iterator.map(ev.write).toList)
      }
    }

  implicit def OptionEncoder[A](
      implicit ev: ConfEncoder[A]
  ): ConfEncoder[Option[A]] =
    new ConfEncoder[Option[A]] {
      override def write(value: Option[A]): Conf = {
        if (value.isDefined) ev.write(value.get)
        else Conf.Null()
      }
    }

  implicit def MapEncoder[A](
      implicit ev: ConfEncoder[A]
  ): ConfEncoder[Map[String, A]] =
    new ConfEncoder[Map[String, A]] {
      override def write(value: Map[String, A]): Conf = {
        Conf.Obj(value.mapValues(ev.write).toList)
      }
    }

}
