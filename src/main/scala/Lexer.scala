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

    def next_token(): Option[Token] = {
        while (current_char.isDefined) {
            if (current_char.get.isLetter || current_char.get.isDigit) {
                return id()
            }
            if (current_char.get == ":" && next_char.getOrElse("") == "=") {
                advance()
                advance()
                return Token(Token.ASSIGN, ":=")
            }
            if (current_char.get == ";") {
                advance()
                return Token(Token.Semicolon, ";")
            }   
            
        }
        return None
    }
}