package skald

import scala.util.Try
import java.time.LocalDate
import java.time.LocalTime

def evalExpr(expr: Expr, data: ShellData): ShellValue = expr match {
  
  // expr = _.size > 100

  case Expr.LitInt(v) => ShellValue.VLong(v)
  case Expr.LitStr(v) => ShellValue.VString(v)
  case Expr.LitBool(v) => ShellValue.VBool(v)
  case Expr.PropAccess(f) => data.getProperty(f).getOrElse(ShellValue.VNone)

  case Expr.GreaterThan(left, right) =>
    (evalExpr(left, data), evalExpr(right, data)) match {
      case (ShellValue.VLong(l), ShellValue.VLong(r))     => ShellValue.VBool(l > r)
      case (ShellValue.VString(l), ShellValue.VString(r)) => ShellValue.VBool(l.compareTo(r) > 0)
 
      case (ShellValue.VDate(l), ShellValue.VDate(r)) => ShellValue.VBool(l.isAfter(r))
      case (ShellValue.VTime(l), ShellValue.VTime(r)) => ShellValue.VBool(l.isAfter(r))

      case (ShellValue.VDate(l), ShellValue.VString(rStr)) =>
        Try(LocalDate.parse(rStr))
          .map(rDate => ShellValue.VBool(l.isAfter(rDate)))
          .getOrElse(ShellValue.VBool(false))

      case (ShellValue.VTime(l), ShellValue.VString(rStr)) =>
        Try(LocalTime.parse(rStr))
          .map(rTime => ShellValue.VBool(l.isAfter(rTime)))
          .getOrElse(ShellValue.VBool(false))

      case _ => ShellValue.VNone 
    }

  case Expr.Equals(left, right) =>
    (evalExpr(left, data), evalExpr(right, data)) match {
      case (ShellValue.VLong(l), ShellValue.VLong(r))     => ShellValue.VBool(l == r)
      case (ShellValue.VString(l), ShellValue.VString(r)) => ShellValue.VBool(l == r)
      case (ShellValue.VBool(l), ShellValue.VBool(r))     => ShellValue.VBool(l == r)

      case (ShellValue.VDate(l), ShellValue.VDate(r))     => ShellValue.VBool(l.equals(r))
      case (ShellValue.VTime(l), ShellValue.VTime(r))     => ShellValue.VBool(l.equals(r))

      case (ShellValue.VDate(l), ShellValue.VString(rStr)) =>
        Try(LocalDate.parse(rStr))
          .map(rDate => ShellValue.VBool(l == rDate))
          .getOrElse(ShellValue.VBool(false))

      case (ShellValue.VTime(l), ShellValue.VString(rStr)) =>
        Try(LocalTime.parse(rStr))
          .map(rTime => ShellValue.VBool(l == rTime))
          .getOrElse(ShellValue.VBool(false))

      case _ => ShellValue.VBool(false)
    }
}
