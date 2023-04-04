package muse

import scala.collection.mutable.ListBuffer

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