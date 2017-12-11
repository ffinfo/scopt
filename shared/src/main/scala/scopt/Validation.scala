package scopt

object Validation {
  def validateValue[A](vs: Seq[A => Either[String, Unit]])(value: A): Either[Seq[String], Unit] = {
    val results = vs map {_.apply(value)}
    (OptionDef.makeSuccess[Seq[String]] /: results) { (acc, r) =>
      (acc match {
        case Right(_) => Seq[String]()
        case Left(xs) => xs
      }) ++ (r match {
        case Right(_) => Seq[String]()
        case Left(x)  => Seq[String](x)
      }) match {
        case Seq()    => acc
        case xs       => Left(xs)
      }
    }
  }
}
