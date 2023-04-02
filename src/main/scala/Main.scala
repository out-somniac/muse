import scala.collection.mutable.ListBuffer

case class Token[T](val token_type: Token.Type, val value: T)
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

// Me lazy me not want error handling
case class BadToken(message: String) extends Error(message)


class Lexer(val text: String) {
    import Token.Type

    def lex(): List[Token[Any]] = {
        val tokens = ListBuffer[Token[Any]]()
        var position = 0
        while( position < text.length) {
            val current_char = text(position)
            if(current_char.isWhitespace) position += 1
            else if (current_char.isDigit) {
                val begin: Int = position
                while(position < text.length && text(position).isDigit) position += 1
                tokens += Token(Type.Numerical, text.substring(begin, position).toInt)
            }
            else if (current_char == '+') {
                position += 1
                tokens += Token(Type.Plus, current_char.toString)
            }    
            else if (current_char == '-') {
                position += 1
                tokens += Token(Type.Minus, current_char.toString)
            }
            else if (current_char == '*') {
                position += 1
                tokens += Token(Type.Multiply, current_char.toString)
            }
            else if (current_char == '/') {
                position += 1
                tokens += Token(Type.Divide, current_char.toString)
            }
            else if (current_char == '(') {
                position += 1
                tokens += Token(Type.LeftParenthesis, current_char.toString)
            }
            else if (current_char == ')') {
                position += 1
                tokens += Token(Type.RightParenthesis, current_char.toString)
            }
            else {
                throw new BadToken("Token not recognized!")
            }
        }
        tokens += Token(Type.EOF, "<EOF>")
        tokens.toList
    }
}

class Interpreter(val tokens: List[Token[Any]]) {
    import Token.Type
    private var token_ptr: Int = 0
    private def current_token: Token[Any] = tokens(token_ptr)

    private def factor(): Int = {
        val token = current_token
        
        if(token.token_type == Type.Numerical) {
            eat(Type.Numerical)
            return token.value.asInstanceOf[Int]
        }
        else if (token.token_type == Type.LeftParenthesis) {
            eat(Type.LeftParenthesis)
            val result = expression()
            eat(Type.RightParenthesis)
            return result
        } else {
            throw new BadToken(s"Bad token type when reading factor: '$token.token_type'")
        }
    }

    private def term(): Int = {
        var result = factor()
        while(current_token.token_type == Type.Multiply || current_token.token_type == Type.Divide) {
            if(current_token.token_type == Type.Multiply) {
                eat(Type.Multiply)
                result = result * factor()
            }
            else if(current_token.token_type == Type.Divide) {
                eat(Type.Divide)
                result = result / factor()
            }
        }
        result
    }

    private def eat(token_type: Type): Unit = {
        if(token_type == current_token.token_type) {
            token_ptr += 1
        } else {
            throw new BadToken("Token not recognized!")
        }
    }

    def expression(): Int = {
        /*
        * expr   : term ((Plus | Minus) term)*
        * term   : factor ((Multiply | Divide) factor)*
        * factor : Numerical | LeftParenthesis expr RightParenthesis  
        */
        var result = this.term()
        while(current_token.token_type == Type.Plus || current_token.token_type == Type.Minus) {
            if(current_token.token_type == Type.Plus) {
                eat(Type.Plus)
                result = result + term()
            }
            else if(current_token.token_type == Type.Minus) {
                eat(Type.Minus)
                result = result - term()
            }
        }
        result
    }
}

@main def main(): Unit = {
    import Token.Type
    val input: String = "(1 + 42) * 3"
    val tokens: List[Token[Any]] = Lexer(input).lex()
    val result = Interpreter(tokens).expression()
    println(result)
}