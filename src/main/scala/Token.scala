package muse

object Token:
    sealed trait Type
    case object LeftParenthesis extends Type
    case object RightParenthesis extends Type
    case object LeftBrace extends Type
    case object RightBrace extends Type
    case object Semicolon extends Type
    case object Plus extends Type
    case object Minus extends Type
    case object Multiply extends Type
    case object Divide extends Type
    case object Assign extends Type
    case object Integer extends Type
    case object Id extends Type
    case object EOF extends Type

class Token(val tpe: Token.Type, val value: String, val line_no: Int, val column: Int) {
    override def toString(): String = {
        return s"Token($tpe, $value)"
    }
}