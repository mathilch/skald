package skald

import java.time.LocalDate
import java.time.LocalTime

enum ShellValue:
  case VLong(v: Long)
  case VString(s: String)
  case VBool(b: Boolean)
  case VDate(d: LocalDate)
  case VTime(t: LocalTime)
  case VNone

