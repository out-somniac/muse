package muse

case class Token[T](val tpe: Token.Type, val value: T)
object Token:
    enum Type:
        case Numerical
        case Plus
        case Minus
        case Multiply
        case Divide
        case EOF
        case LeftParenthesis
        case RightParenthesis