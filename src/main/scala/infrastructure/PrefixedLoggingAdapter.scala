package infrastructure

import akka.event.LoggingAdapter

class PrefixedLoggingAdapter(prefix: String, log: LoggingAdapter) extends LoggingAdapter {
  override def isErrorEnabled = log.isErrorEnabled
  override def isWarningEnabled = log.isWarningEnabled
  override def isInfoEnabled = log.isInfoEnabled
  override def isDebugEnabled = log.isDebugEnabled

  @inline private[this] def prefixed(message: String) = s"$prefix$message"

  override protected def notifyError(message: String) = log.error(prefixed(message))
  override protected def notifyError(cause: Throwable, message: String) =
    log.error(cause, prefixed(message))
  override protected def notifyWarning(message: String) = log.warning(prefixed(message))
  override protected def notifyInfo(message: String) = log.info(prefixed(message))
  override protected def notifyDebug(message: String) = log.debug(prefixed(message))
}
