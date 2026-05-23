package skald

def evalExpr(expr: Expr, data: ShellData): ShellValue = expr match {
  
  // expr = _.size > 100

  case Expr.LitInt(v) => ShellValue.VLong(v)
  case Expr.LitStr(v) => ShellValue.VString(v)
  case Expr.LitBool(v) => ShellValue.VBool(v)
  case Expr.PropAccess(f) => data.getProperty(f).getOrElse(ShellValue.VNone)

  case Expr.GreaterThan(left, right) =>
    (evalExpr(left, data), evalExpr(right, data)) match {
      case (ShellValue.VLong(l), ShellValue.VLong(r)) => ShellValue.VBool(l > r)
      case _ => ShellValue.VNone 
    }

  case Expr.Equals(left, right) =>
    (evalExpr(left, data), evalExpr(right, data)) match {
      case (ShellValue.VLong(l), ShellValue.VLong(r))     => ShellValue.VBool(l == r)
      case (ShellValue.VString(l), ShellValue.VString(r)) => ShellValue.VBool(l == r)
      case (ShellValue.VBool(l), ShellValue.VBool(r))     => ShellValue.VBool(l == r)
      case _ => ShellValue.VBool(false)
    }
}
