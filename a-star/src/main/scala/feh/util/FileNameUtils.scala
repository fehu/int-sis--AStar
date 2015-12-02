package feh.util

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object FileNameUtils {

  def formatDateFile(prefix: String, suffix: String, time: LocalDateTime = LocalDateTime.now) = {
    lazy val dateFormat = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    prefix + time.format(dateFormat) + suffix
  }

}
