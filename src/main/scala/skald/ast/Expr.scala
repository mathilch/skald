package skald

enum Expr:
  case PropAccess(field: String)
  case LitInt(value: Long)
  case LitStr(value: String)
  case LitBool(value: Boolean)

  case GreaterThan(left: Expr, right: Expr)
  case Equals(left: Expr, right: Expr)
