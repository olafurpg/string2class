package metaconfig.internal

/**
  * Copied from https://github.com/monix/implicitbox/blob/master/shared/src/main/scala/implicitbox/Priority.scala
  * */
private[metaconfig] sealed trait Priority[+P, +F] {
  import Priority.{Fallback, Preferred}

  def fold[B](f1: P => B)(f2: F => B): B =
    this match {
      case Preferred(x) => f1(x)
      case Fallback(y) => f2(y)
    }
}

private[metaconfig] object Priority extends FindPreferred {
  final case class Preferred[P](get: P) extends Priority[P, Nothing]
  final case class Fallback[F](get: F) extends Priority[Nothing, F]

  def apply[P, F](implicit ev: Priority[P, F]): Priority[P, F] = ev
}

private[metaconfig] trait FindPreferred extends FindFallback {
  implicit def preferred[P](implicit ev: P): Priority[P, Nothing] =
    Priority.Preferred(ev)
}

private[metaconfig] trait FindFallback {
  implicit def fallback[F](implicit ev: F): Priority[Nothing, F] =
    Priority.Fallback(ev)
}
