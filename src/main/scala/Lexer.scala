package muse

class Lexer(val code: String) {
    private var index = 0;
    
    def current_char: Option[Char] = {
        if (index >= code.length) {
            return None
        }
        return Some(code.charAt(index))
    }

    def next_char: Option[Char] = {
        if (index + 1 >= code.length) {
            return None
        }
        return Some(code.charAt(index + 1))
    }

    def advance(): Unit = {
        index += 1
    }

    def skip_whitespace(): Unit = {
        while(current_char.isDefined && current_char.get.isWhitespace) {
            advance()
        }
    }

    def integer(): Token = {
        var result: String = ""
        while (current_char.isDefined && current_char.get.isDigit) {
            result += current_char.get
            advance()
        }
        return Token(Token.Integer, result)
    }

    def id(): Token = {
        var result: String = ""
        while (current_char.isDefined && current_char.get.isLetterOrDigit) {
            result += current_char.get
            advance() 
        }
        // Check if result is a reserved keyword
        return Token(Token.Id, result)
    }

    def next_token(): Token = {
        while (current_char.isDefined) {
            if (current_char.get.isWhitespace) {
                skip_whitespace()
            }   

            val current: Char = current_char.get
            if (current.isLetter) {
                return id()
            }
            if (current.isDigit) {
                return integer()
            }
            if (current == ':' && next_char.getOrElse(" ") == '=') {
                advance()
                advance()
                return Token(Token.Assign, ":=")
            }

            advance()
            current match {
                case ';' => return Token(Token.Semicolon, ";")
                case '+' => return Token(Token.Plus, "+")
                case '-' => return Token(Token.Minus, "-")
                case '*' => return Token(Token.Multiply, "*")
                case '/' => return Token(Token.Divide, "/")
                case '(' => return Token(Token.LeftParenthesis, "(")
                case ')' => return Token(Token.RightParenthesis, ")")
                case '{' => return Token(Token.LeftBrace, "{")
                case '}' => return Token(Token.RightBrace, "}") 
            }
            // TODO: Proper error handling
            println("Did not expect this character")
        }
        return Token(Token.EOF, "EOF")
    }
}