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
    compareValues(evalExpr(left, data), evalExpr(right, data)) match {
      case Some(res) => ShellValue.VBool(res > 0)
      case None      => ShellValue.VNone
    }

  case Expr.LesserThan(left, right) =>
    compareValues(evalExpr(left, data), evalExpr(right, data)) match {
      case Some(res) => ShellValue.VBool(res < 0)
      case None      => ShellValue.VNone
    }

  case Expr.Equals(left, right) =>
    compareValues(evalExpr(left, data), evalExpr(right, data)) match {
      case Some(res) => ShellValue.VBool(res == 0)
      case None      => ShellValue.VBool(false) 
    }
}

def compareValues(leftVal: ShellValue, rightVal: ShellValue): Option[Int] = (leftVal, rightVal) match {
  case (ShellValue.VLong(l), ShellValue.VLong(r))     => Some(l.compareTo(r))
  case (ShellValue.VString(l), ShellValue.VString(r)) => Some(l.compareTo(r))
  case (ShellValue.VBool(l), ShellValue.VBool(r))     => Some(l.compareTo(r))

  // Dato og tid
  case (ShellValue.VDate(l), ShellValue.VDate(r))     => Some(l.compareTo(r))
  case (ShellValue.VTime(l), ShellValue.VTime(r))     => Some(l.compareTo(r))

  // Smart Coercion for Dato
  case (ShellValue.VDate(l), ShellValue.VString(rStr)) =>
    Try(LocalDate.parse(rStr)).toOption.map(rDate => l.compareTo(rDate))

  // Smart Coercion for Tid
  case (ShellValue.VTime(l), ShellValue.VString(rStr)) =>
    Try(LocalTime.parse(rStr)).toOption.map(rTime => l.compareTo(rTime))

  case _ => None // Ugyldig sammenligning
}
