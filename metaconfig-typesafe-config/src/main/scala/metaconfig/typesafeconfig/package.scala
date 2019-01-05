package metaconfig

package object typesafeconfig {
  implicit val typesafeConfigMetaconfigParser = new MetaconfigParser {
    override def fromInput(input: Input): Configured[Conf] = input match {
      case Input.File(path, _, _) =>
        TypesafeConfig2Class.gimmeConfFromFile(path.toFile)
      case els =>
        TypesafeConfig2Class.gimmeConfFromString(new String(els.chars))
    }
  }
}
