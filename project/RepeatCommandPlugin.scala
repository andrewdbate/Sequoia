import sbt._
import Keys._

import java.io.{PrintWriter, StringWriter}
import java.text.DateFormat
import java.util.Date
import java.util.logging.{Handler, LogRecord, Level, FileHandler, Formatter, Logger, ConsoleHandler}

object RepeatCommandPlugin extends AutoPlugin {

  private[this] object RepeatTaskFormatter {
    /**
     * The character sequence that is used to separate lines in the generated stream.
     */
    val lineSep: String = System.lineSeparator
  }

  /**
   * A <code>RepeatTaskFormatter</code> formats log records into short human-readable messages.
   */
  private[this] class RepeatTaskFormatter extends Formatter {
    import RepeatTaskFormatter.lineSep

    /**
     * An instance of a DateFormatter that is used for formatting
     * the time of a log record into a human-readable string,
     * according to the rules of the current locale.
     */
    private[this] val dateFormat = DateFormat.getDateTimeInstance

    private[this] val date = new Date

    /**
     * Formats a log record into a String.
     *
     * @param record the log record to be formatted
     * @return a short human-readable message.  Lines are separated using the default platform line separator
     * @throws NullPointerException if <code>record</code> is <code>null</code>
     */
    override def format(record: LogRecord): String = {
      val buf = new StringBuilder

      date.setTime(record.getMillis)

      buf.append(dateFormat.format(date))
      buf.append(": ")
      
      val level: Level = record.getLevel
      if (level.intValue <= Level.INFO.intValue) {
        buf.append(formatMessage(record))
      } else {
        buf.append(lineSep)
        buf.append(record.getLevel)
        buf.append(": ")
        buf.append(formatMessage(record))
      }
     
      buf.append(lineSep)

      val throwable: Throwable = record.getThrown
      if (throwable != null) {
        val sw = new StringWriter
        throwable.printStackTrace(new PrintWriter(sw, true))
        buf.append(sw.toString)
      }

      buf.result
    }
  }


  /**
   * Some((startTime, numRuns)) = startTimeAndNumRuns is an optional tuple whose first component is the start time
   * for the repeat task and whose second component is the number of times the task has been run.
   */
  private[this] var startTimeAndNumRuns: Option[(Long, Int)] = None

  private[this] val logger: Logger = Logger.getLogger(getClass.getName)
  // We want to output to the console, but using our customer formatter, and hence we do not use the default parent handler.
  logger.setUseParentHandlers(false)
  private[this] val consoleHandler = new ConsoleHandler
  private[this] val formatter = new RepeatTaskFormatter
  consoleHandler.setFormatter(formatter)
  

  private[this] def setLoggerHandler(handler: Handler) {
    /* Remove all old handlers in case the repat task crashed and didn't clean up after itself.
     * We don't want to append to an old log.
     */
    clearHandlers
    logger.addHandler(handler)
    logger.addHandler(consoleHandler)
  }

  private[this] def clearHandlers {
    for (handler <- logger.getHandlers) {
      logger.removeHandler(handler)
      handler.close
    }
  }

  private[this] def getLoggerHandler(commandName: String): Handler = {
    // Use command as filename, replacing spaces with dash, and replacing illegal characters (and possibly some others, but that is platform dependent) with underscore
    val fh = new FileHandler(s"""./${commandName.replace(' ','-').replaceAll("[^a-zA-Z0-9\\.\\-]", "_")}-${System.nanoTime}.log""")
    fh.setFormatter(formatter)
    fh
  }

  private[this] val InternalRepeatCleanUp = "internalRepeatCleanUp"

  /**
   * This is an internal auxillary command that will clean up after a repeat command
   * in the case when it fails, and log that a failure has occurred.
   * The argument to this command should be the repeat command that caused the failure.
   */
  private[this] def internalRepeatCleanUp: Command = Command.single(InternalRepeatCleanUp){
    (state: State, repeat: String) =>
      cleanUp(true, repeat)
      // Return to SBT shell after failure
      //Command.process(BasicCommandStrings.Shell, s)
      BasicCommandStrings.Shell :: state
  }

  /**
   * Resets local state after each run of a repeated command.
   *
   * @param failure  set to true if the last run of the task failed, otherwise false
   * @param repeat   the command to be repated
   */
  private[this] def cleanUp(failure: Boolean, repeat: String) {
    if (failure)
      logger.info(s"Command '$repeat' terminated with failure.")
    else
      logger.info(s"Command '$repeat' terminated by user (a key was pressed).")
    startTimeAndNumRuns = None
    // Clear log handler set
    clearHandlers
  }

  def executeUntilFail(state: State, next: String, repeat: String): State = {

    /** Prints the time taken for this command in hours, minutes and seconds */
    def printTime(millis: Long, numRuns: Int) {
      import java.util.concurrent.TimeUnit
      val numDays: Long    = TimeUnit.MILLISECONDS.toDays(millis)
      val numHours: Long   = TimeUnit.MILLISECONDS.toHours(millis) - TimeUnit.DAYS.toHours(TimeUnit.MILLISECONDS.toDays(millis))
      val numMinutes: Long = TimeUnit.MILLISECONDS.toMinutes(millis) - TimeUnit.HOURS.toMinutes(TimeUnit.MILLISECONDS.toHours(millis))
      val numSeconds: Long = TimeUnit.MILLISECONDS.toSeconds(millis) - TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(millis))
      def makePlural(word: String, count: Long) = s"""$word${if (count != 1) "s" else ""}"""
      val times = makePlural("time", numRuns)
      val days = makePlural("day", numDays)
      val hours = makePlural("hour", numHours)
      val minutes = makePlural("minute", numMinutes)
      val seconds = makePlural("second", numSeconds)
      val result: String =
        if (numDays > 0) {
          s"Command '$repeat' executed %1d $times and took %1d $days %1d $hours %1d $minutes %1d $seconds.".format(numRuns, numDays, numHours, numMinutes, numSeconds)
        } else if (numHours > 0) {
          s"Command '$repeat' executed %1d $times and took %1d $hours %1d $minutes %1d $seconds.".format(numRuns, numHours, numMinutes, numSeconds)
        } else if (numMinutes > 0) {
          s"Command '$repeat' executed %1d $times and took %1d $minutes %1d $seconds.".format(numRuns, numMinutes, numSeconds)
        } else if (numSeconds > 0) {
          s"Command '$repeat' executed %1d $times and took %1d $seconds.".format(numRuns, numSeconds)
        } else {
          s"Command '$repeat' executed %1d $times and took %1d milliseconds.".format(numRuns, millis)
        }
      logger.info(result)
    }

    startTimeAndNumRuns = startTimeAndNumRuns match {
      case Some((startTime, numRuns)) =>
        // If start time recorded, then just update the count of the number of times the task has been run
        printTime(System.currentTimeMillis - startTime, numRuns + 1)
        Some((startTime, numRuns + 1))
      case None =>
        setLoggerHandler(getLoggerHandler(repeat))
        logger.info(s"Command '$repeat' started.")
        // Start at zero since we increment on the terminating iteration
        Some((System.currentTimeMillis, 0))
    }

    /** Returns {@code true} if the repeated task should terminate */
    def shouldTerminate: Boolean = {
      var result = false
      while (System.in.available > 0) {
        System.in.read
        result = true
      }
      result
    }

    import sbt.BasicCommandStrings._
    if (shouldTerminate) {
      cleanUp(false, repeat)
      state
    } else {
      println("Press any key to cancel the repeat command...")
      (next :: FailureWall :: repeat :: state).copy(onFailure = Some(Exec(s"$InternalRepeatCleanUp $repeat", state.source)))
    }
  }

  val RepeatExecutePrefix = "repeat"
  def repeatBriefHelp = (RepeatExecutePrefix + " <command>", repeatDetail)
  def repeatDetail = "Repeatedly executes the specified command until it fails or a key is pressed, at which point execution terminates when the last run command terminates."

  def repeatUntilFailCommand =
    Command(RepeatExecutePrefix, repeatBriefHelp, repeatDetail)(sbt.BasicCommands.otherCommandParser) { (s, arg) =>
      val repeat = RepeatExecutePrefix + (if (arg.startsWith(" ")) arg else " " + arg)
      executeUntilFail(s, arg, repeat)
    }

  lazy val repeatCommandSettings: Seq[sbt.Def.Setting[_]] = Seq(
    commands ++= Seq(internalRepeatCleanUp, repeatUntilFailCommand)
  )

}
