package scopt

import scala.collection.mutable.{ListBuffer, ListMap}

class OptionParser {

}
/** <code>scopt.immutable.OptionParser</code> is instantiated within your object,
  * set up by an (ordered) sequence of invocations of
  * the various builder methods such as
  * <a href="#opt[A](Char,String)(Read[A]):OptionDef[A,C]"><code>opt</code></a> method or
  * <a href="#arg[A](String)(Read[A]):OptionDef[A,C]"><code>arg</code></a> method.
  * {{{
  * val parser = new scopt.OptionParser[Config]("scopt") {
  *   head("scopt", "3.x")
  *
  *   opt[Int]('f', "foo").action( (x, c) =>
  *     c.copy(foo = x) ).text("foo is an integer property")
  *
  *   opt[File]('o', "out").required().valueName("<file>").
  *     action( (x, c) => c.copy(out = x) ).
  *     text("out is a required file property")
  *
  *   opt[(String, Int)]("max").action({
  *       case ((k, v), c) => c.copy(libName = k, maxCount = v) }).
  *     validate( x =>
  *       if (x._2 > 0) success
  *       else failure("Value <max> must be >0") ).
  *     keyValueName("<libname>", "<max>").
  *     text("maximum count for <libname>")
  *
  *   opt[Seq[File]]('j', "jars").valueName("<jar1>,<jar2>...").action( (x,c) =>
  *     c.copy(jars = x) ).text("jars to include")
  *
  *   opt[Map[String,String]]("kwargs").valueName("k1=v1,k2=v2...").action( (x, c) =>
  *     c.copy(kwargs = x) ).text("other arguments")
  *
  *   opt[Unit]("verbose").action( (_, c) =>
  *     c.copy(verbose = true) ).text("verbose is a flag")
  *
  *   opt[Unit]("debug").hidden().action( (_, c) =>
  *     c.copy(debug = true) ).text("this option is hidden in the usage text")
  *
  *   help("help").text("prints this usage text")
  *
  *   arg[File]("<file>...").unbounded().optional().action( (x, c) =>
  *     c.copy(files = c.files :+ x) ).text("optional unbounded args")
  *
  *   note("some notes.".newline)
  *
  *   cmd("update").action( (_, c) => c.copy(mode = "update") ).
  *     text("update is a command.").
  *     children(
  *       opt[Unit]("not-keepalive").abbr("nk").action( (_, c) =>
  *         c.copy(keepalive = false) ).text("disable keepalive"),
  *       opt[Boolean]("xyz").action( (x, c) =>
  *         c.copy(xyz = x) ).text("xyz is a boolean property"),
  *       opt[Unit]("debug-update").hidden().action( (_, c) =>
  *         c.copy(debug = true) ).text("this option is hidden in the usage text"),
  *       checkConfig( c =>
  *         if (c.keepalive && c.xyz) failure("xyz cannot keep alive")
  *         else success )
  *     )
  * }
  *
  * // parser.parse returns Option[C]
  * parser.parse(args, Config()) match {
  *   case Some(config) =>
  *     // do stuff
  *
  *   case None =>
  *     // arguments are bad, error message will have been displayed
  * }
  * }}}
  */
abstract class OptionParser[C](programName: String) {
  protected val options = new ListBuffer[OptionDef[_, C]]
  protected val helpOptions = new ListBuffer[OptionDef[_, C]]

  import platform._

  def errorOnUnknownArgument: Boolean = true
  def showUsageOnError: Boolean = helpOptions.isEmpty
  def renderingMode: RenderingMode = RenderingMode.TwoColumns
  def terminate(exitState: Either[String, Unit]): Unit =
    exitState match {
      case Left(_)  => sys.exit(1)
      case Right(_) => sys.exit(0)
    }

  def reportError(msg: String): Unit = {
    Console.err.println("Error: " + msg)
  }

  def reportWarning(msg: String): Unit = {
    Console.err.println("Warning: " + msg)
  }

  def showTryHelp(): Unit = {
    def oxford(xs: List[String]): String = xs match {
      case a :: b :: Nil => a + " or " + b
      case _             => (xs.dropRight(2) :+ xs.takeRight(2).mkString(", or ")).mkString(", ")
    }
    Console.err.println("Try " + oxford(helpOptions.toList map {_.fullName}) + " for more information.")
  }

  /** adds usage text. */
  def head(xs: String*): OptionDef[Unit, C] = makeDef[Unit](Head, "") text(xs.mkString(" "))

  /** adds an option invoked by `--name x`.
    * @param name name of the option
    */
  def opt[A: Read](name: String): OptionDef[A, C] = makeDef(Opt, name)

  /** adds an option invoked by `-x value` or `--name value`.
    * @param x name of the short option
    * @param name name of the option
    */
  def opt[A: Read](x: Char, name: String): OptionDef[A, C] =
    opt[A](name) abbr(x.toString)

  /** adds usage text. */
  def note(x: String): OptionDef[Unit, C] = makeDef[Unit](Note, "") text(x)

  /** adds an argument invoked by an option without `-` or `--`.
    * @param name name in the usage text
    */
  def arg[A: Read](name: String): OptionDef[A, C] = makeDef(Arg, name) required()

  /** adds a command invoked by an option without `-` or `--`.
    * @param name name of the command
    */
  def cmd(name: String): OptionDef[Unit, C] = makeDef[Unit](Cmd, name)

  /** adds an option invoked by `--name` that displays usage text and exits.
    * @param name name of the option
    */
  def help(name: String): OptionDef[Unit, C] = {
    val o = opt[Unit](name) action { (x, c) =>
      showUsage()
      terminate(Right(()))
      c
    }
    helpOptions += o
    o
  }


  /** adds an option invoked by `--name` that displays header text and exits.
    * @param name name of the option
    */
  def version(name: String): OptionDef[Unit, C] =
    opt[Unit](name) action { (x, c) =>
      showHeader()
      terminate(Right(()))
      c
    }

  /** adds final check. */
  def checkConfig(f: C => Either[String, Unit]): OptionDef[Unit, C] =
    makeDef[Unit](Check, "") validateConfig(f)

  def showHeader() {
    Console.out.println(header)
  }
  def header: String = {
    import OptionDef._
    (heads map {_.usage}).mkString(NL)
  }

  def showUsage(): Unit = {
    Console.out.println(usage)
  }
  def showUsageAsError(): Unit = {
    Console.err.println(usage)
  }
  def usage: String = renderUsage(renderingMode)
  def renderUsage(mode: RenderingMode): String =
    mode match {
      case RenderingMode.OneColumn => renderOneColumnUsage
      case RenderingMode.TwoColumns => renderTwoColumnsUsage
    }
  def renderOneColumnUsage: String = {
    import OptionDef._
    val descriptions = optionsForRender map {_.usage}
    (if (header == "") "" else header + NL) +
      "Usage: " + usageExample + NLNL +
      descriptions.mkString(NL)
  }
  def renderTwoColumnsUsage: String = {
    import OptionDef._
    val xs = optionsForRender
    val descriptions = {
      val col1Len = math.min(column1MaxLength, xs map {_.usageColumn1.length + WW.length} match {
        case Nil => 0
        case list => list.max
      })
      xs map {_.usageTwoColumn(col1Len)}
    }
    (if (header == "") "" else header + NL) +
      "Usage: " + usageExample + NLNL +
      descriptions.mkString(NL)
  }
  def optionsForRender: List[OptionDef[_, C]] = {
    val unsorted = options filter { o => o.kind != Head && o.kind != Check && !o.isHidden }
    val (remaining, sorted) = unsorted partition {_.hasParent} match {
      case (p, np) => (ListBuffer() ++ p, ListBuffer() ++ np)
    }
    var continue = true
    while (!remaining.isEmpty && continue) {
      continue = false
      for {
        parent <- sorted
      } {
        val childrenOfThisParent = remaining filter {_.getParentId == Some(parent.id)}
        if (childrenOfThisParent.nonEmpty) {
          remaining --= childrenOfThisParent
          sorted.insertAll((sorted indexOf parent) + 1, childrenOfThisParent)
          continue = true
        }
      }
    }
    sorted.toList
  }
  def usageExample: String = commandExample(None)
  private[scopt] def commandExample(cmd: Option[OptionDef[_, C]]): String = {
    val text = new ListBuffer[String]()
    text += cmd map {commandName} getOrElse programName
    val parentId = cmd map {_.id}
    val cs = commands filter {_.getParentId == parentId}
    if (cs.nonEmpty) text += cs map {_.name} mkString("[", "|", "]")
    val os = options.toSeq filter { case x => x.kind == Opt && x.getParentId == parentId }
    val as = arguments filter {_.getParentId == parentId}
    if (os.nonEmpty) text += "[options]"
    if (cs exists { case x => arguments exists {_.getParentId == Some(x.id)}}) text += "<args>..."
    else if (as.nonEmpty) text ++= as map {_.argName}
    text.mkString(" ")
  }
  private[scopt] def commandName(cmd: OptionDef[_, C]): String =
    (cmd.getParentId map { x =>
      (commands find {_.id == x} map {commandName} getOrElse {""}) + " "
    } getOrElse {""}) + cmd.name

  /** call this to express success in custom validation. */
  def success: Either[String, Unit] = OptionDef.makeSuccess[String]
  /** call this to express failure in custom validation. */
  def failure(msg: String): Either[String, Unit] = Left(msg)

  protected def heads: Seq[OptionDef[_, C]] = options.toSeq filter {_.kind == Head}
  protected def nonArgs: Seq[OptionDef[_, C]] = options.toSeq filter { case x => x.kind == Opt || x.kind == Note }
  protected def arguments: Seq[OptionDef[_, C]] = options.toSeq filter {_.kind == Arg}
  protected def commands: Seq[OptionDef[_, C]] = options.toSeq filter {_.kind == Cmd}
  protected def checks: Seq[OptionDef[_, C]] = options.toSeq filter {_.kind == Check}
  protected def makeDef[A: Read](kind: OptionDefKind, name: String): OptionDef[A, C] =
    updateOption(new OptionDef[A, C](parser = this, kind = kind, name = name))
  private[scopt] def updateOption[A: Read](option: OptionDef[A, C]): OptionDef[A, C] = {
    val idx = options indexWhere { _.id == option.id }
    if (idx > -1) options(idx) = option
    else options += option
    option
  }

  /** parses the given `args`.
    * @return `true` if successful, `false` otherwise
    */
  def parse(args: Seq[String])(implicit ev: Zero[C]): Boolean =
    parse(args, ev.zero) match {
      case Some(x) => true
      case None    => false
    }

  /** parses the given `args`.
    */
  def parse(args: Seq[String], init: C): Option[C] = {
    var i = 0
    val pendingOptions = ListBuffer() ++ (nonArgs filterNot {_.hasParent})
    val pendingArgs = ListBuffer() ++ (arguments filterNot {_.hasParent})
    val pendingCommands = ListBuffer() ++ (commands filterNot {_.hasParent})
    val occurrences = ListMap[OptionDef[_, C], Int]().withDefaultValue(0)
    var _config: C = init
    var _error = false

    def pushChildren(opt: OptionDef[_, C]): Unit = {
      // commands are cleared to guarantee that it appears first
      pendingCommands.clear()

      pendingOptions insertAll (0, nonArgs filter { x => x.getParentId == Some(opt.id) &&
        !pendingOptions.contains(x) })
      pendingArgs insertAll (0, arguments filter { x => x.getParentId == Some(opt.id) &&
        !pendingArgs.contains(x) })
      pendingCommands insertAll (0, commands filter { x => x.getParentId == Some(opt.id) &&
        !pendingCommands.contains(x) })
    }
    def handleError(msg: String): Unit = {
      if (errorOnUnknownArgument) {
        _error = true
        reportError(msg)
      }
      else reportWarning(msg)
    }
    def handleArgument(opt: OptionDef[_, C], arg: String): Unit = {
      opt.applyArgument(arg, _config) match {
        case Right(c) =>
          _config = c
          pushChildren(opt)
        case Left(xs) =>
          _error = true
          xs foreach reportError
      }
    }
    def handleOccurrence(opt: OptionDef[_, C], pending: ListBuffer[OptionDef[_, C]]): Unit = {
      occurrences(opt) += 1
      if (occurrences(opt) >= opt.getMaxOccurs) {
        pending -= opt
      }
    }
    def findCommand(cmd: String): Option[OptionDef[_, C]] =
      pendingCommands find {_.name == cmd}
    // greedy match
    def handleShortOptions(g0: String): Unit = {
      val gs =  (0 to g0.size - 1).toSeq map { n => g0.substring(0, g0.size - n) }
      gs flatMap { g => pendingOptions map {(g, _)} } find { case (g, opt) =>
        opt.shortOptTokens("-" + g) == 1
      } match {
        case Some(p) =>
          val (g, option) = p
          handleOccurrence(option, pendingOptions)
          handleArgument(option, "")
          if (g0.drop(g.size) != "") {
            handleShortOptions(g0 drop g.size)
          }
        case None => handleError("Unknown option " + "-" + g0)
      }
    }
    def handleChecks(c: C): Unit = {
      Validation.validateValue(checks flatMap {_.checks})(c) match {
        case Right(c) => // do nothing
        case Left(xs) =>
          _error = true
          xs foreach reportError
      }
    }
    while (i < args.length) {
      pendingOptions find {_.tokensToRead(i, args) > 0} match {
        case Some(option) =>
          handleOccurrence(option, pendingOptions)
          option(i, args) match {
            case Right(v) =>          handleArgument(option, v)
            case Left(outOfBounds) => handleError(outOfBounds)
          }
          // move index forward for gobbling
          if (option.tokensToRead(i, args) > 1) {
            i += option.tokensToRead(i, args) - 1
          } // if
        case None =>
          args(i) match {
            case arg if arg startsWith "--" => handleError("Unknown option " + arg)
            case arg if arg startsWith "-"  =>
              if (arg == "-") handleError("Unknown option " + arg)
              else handleShortOptions(arg drop 1)
            case arg if findCommand(arg).isDefined =>
              val cmd = findCommand(arg).get
              handleOccurrence(cmd, pendingCommands)
              handleArgument(cmd, "")
            case arg if pendingArgs.isEmpty => handleError("Unknown argument '" + arg + "'")
            case arg =>
              val first = pendingArgs.head
              handleOccurrence(first, pendingArgs)
              handleArgument(first, arg)
          }
      }
      i += 1
    }

    pendingOptions.filter(_.hasFallback).foreach { opt =>
      val fallback = opt.getFallback
      if (fallback != null) {
        handleOccurrence(opt, pendingOptions)
        handleArgument(opt, fallback.toString)
      }
    }
    (pendingOptions filter { opt => opt.getMinOccurs > occurrences(opt) }) foreach { opt =>
      if (opt.getMinOccurs == 1) reportError("Missing " + opt.shortDescription)
      else reportError(opt.shortDescription.capitalize + " must be given " + opt.getMinOccurs + " times")
      _error = true
    }
    (pendingArgs filter { arg => arg.getMinOccurs > occurrences(arg) }) foreach { arg =>
      if (arg.getMinOccurs == 1) reportError("Missing " + arg.shortDescription)
      else reportError(arg.shortDescription.capitalize + "' must be given " + arg.getMinOccurs + " times")
      _error = true
    }
    handleChecks(_config)
    if (_error) {
      if (showUsageOnError) showUsageAsError()
      else showTryHelp()
      None
    }
    else Some(_config)
  }
}
