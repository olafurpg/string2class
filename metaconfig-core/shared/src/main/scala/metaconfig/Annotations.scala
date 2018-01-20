package metaconfig

import scala.annotation.StaticAnnotation

final case class ExtraName(value: String) extends StaticAnnotation
final case class DeprecatedName(
    name: String,
    message: String,
    sinceVersion: String)
    extends StaticAnnotation {
  override def toString: String =
    s"Setting '$name' is deprecated since version $sinceVersion. $message"
}
final case class ExampleValue(value: String) extends StaticAnnotation
final case class Description(value: String) extends StaticAnnotation
final case class SinceVersion(value: String) extends StaticAnnotation
final case class Deprecated(message: String, since: String)
    extends StaticAnnotation
