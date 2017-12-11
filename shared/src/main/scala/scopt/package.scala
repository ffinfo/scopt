package object scopt {
  private[scopt] sealed trait OptionDefKind {}
  private[scopt] case object Opt extends OptionDefKind
  private[scopt] case object Note extends OptionDefKind
  private[scopt] case object Arg extends OptionDefKind
  private[scopt] case object Cmd extends OptionDefKind
  private[scopt] case object Head extends OptionDefKind
  private[scopt] case object Check extends OptionDefKind
}
