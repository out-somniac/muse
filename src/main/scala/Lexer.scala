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

    def skip_comment(): Unit = {
        while(current_char.getOrElse('_') != '*' || next_char.getOrElse('_') != '/') {
            advance()
        }
        advance()
        advance()
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
        // Check if result is a reserved keyword otherise return identifier
        return Token(Token.Id, result, line_no, column)
    }

    def skip_if_possible(): Boolean = {
        if (current_char.get.isWhitespace) {
            skip_whitespace()
            return true
        }
        if (current_char.get == '/' && next_char.isDefined && next_char.get == '*') {
            skip_comment()
            return true
        }
        return false
    }
        


    def next_token(): Token = {
        while (current_char.isDefined) {
            while (skip_if_possible()) {}

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
                case  _  => throw LexerException(s"[Lexer]$line_no:$column -> Unexpected character \"${current_char.get}\"")  
            }
        }
        return Token(Token.EOF, "EOF", line_no, column)
    }
}