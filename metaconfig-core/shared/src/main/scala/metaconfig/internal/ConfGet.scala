package metaconfig.internal

import scala.collection.mutable
import metaconfig.{Conf, ConfDecoder, ConfError, Configured, WithDefault}
import metaconfig.ConfDecoder.ConfDecoderWithDefaultMaybe

object ConfGet {
  def getKey(obj: Conf, keys: Seq[String]): Option[Conf] =
    if (keys.isEmpty) None
    else {
      obj match {
        case obj @ Conf.Obj(_) =>
          obj.values
            .collectFirst { case (key, value) if key == keys.head => value }
            .orElse(getKey(obj, keys.tail))
        case _ => None
      }
    }

  def getOrElse[T](conf: Conf, default: T, path: String, extraNames: String*)(
      implicit ev: ConfDecoderWithDefaultMaybe[T]
  ): Configured[T] = {
    getKey(conf, path +: extraNames) match {
      case Some(value) =>
        ev.fold(_.decoder(WithDefault.of(default)).read(value))(_.read(value))
      case None => Configured.Ok(default)
    }
  }

  def get[T](conf: Conf, path: String, extraNames: String*)(
      implicit ev: ConfDecoder[T]
  ): Configured[T] = {
    getKey(conf, path +: extraNames) match {
      case Some(value) => ev.read(value)
      case None =>
        conf match {
          case obj @ Conf.Obj(_) => ConfError.missingField(obj, path).notOk
          case _ =>
            ConfError
              .typeMismatch(s"Conf.Obj with field $path", conf, path)
              .notOk
        }
    }
  }

  // Copy-pasted from scala.meta inputs because it's private.
  // TODO(olafur) expose utility in inputs to get offset from line
  private[metaconfig] def getOffsetByLine(chars: Array[Char]): Array[Int] = {
    val buf = new mutable.ArrayBuffer[Int]
    buf += 0
    var i = 0
    while (i < chars.length) {
      if (chars(i) == '\n') buf += (i + 1)
      i += 1
    }
    if (buf.last != chars.length)
      buf += chars.length // sentinel value used for binary search
    buf.toArray
  }

}
