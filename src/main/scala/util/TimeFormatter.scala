package util

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object TimeFormatter {
  def getFormattedCurrentDate: String =
    val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    LocalDate.now().format(formatter)
}
