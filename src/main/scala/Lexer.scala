package muse

class Lexer(val code: String) {
    private var index = 0
    private var line_no = 1
    private var column = 0

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
        column += 1
    }

    def skip_whitespace(): Unit = {
        while(current_char.isDefined && current_char.get.isWhitespace) {
            if (current_char.get == '\n') {
                line_no += 1
                column = -1
            }
            advance()
        }
    }

    def integer(): Token = {
        var result: String = ""
        while (current_char.isDefined && current_char.get.isDigit) {
            result += current_char.get
            advance()
        }
        return Token(Token.Integer, result, line_no, column)
    }

    def id(): Token = {
        var result: String = ""
        while (current_char.isDefined && current_char.get.isLetterOrDigit) {
            result += current_char.get
            advance() 
        }
        // Check if result is a reserved keyword
        return Token(Token.Id, result, line_no, column)
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
                return Token(Token.Assign, ":=", line_no, column)
            }

            advance()
            current match {
                case ';' => return Token(Token.Semicolon, ";", line_no, column)
                case '+' => return Token(Token.Plus, "+", line_no, column)
                case '-' => return Token(Token.Minus, "-", line_no, column)
                case '*' => return Token(Token.Multiply, "*", line_no, column)
                case '/' => return Token(Token.Divide, "/", line_no, column)
                case '(' => return Token(Token.LeftParenthesis, "(", line_no, column)
                case ')' => return Token(Token.RightParenthesis, ")", line_no, column)
                case '{' => return Token(Token.LeftBrace, "{", line_no, column)
                case '}' => return Token(Token.RightBrace, "}", line_no, column) 
            }
            // TODO: Proper error handling
            println("Did not expect this character")
        }
        return Token(Token.EOF, "EOF", line_no, column)
    }
}