enum Token:
    case Integer(value: Int)
    case Plus
    case Minus
    case EOF


class Tokenizer(val text: String) {
    private var position: Int = 0

    private def current_char: Option[Char] = {
        if( this.position >= this.text.length() ) None 
        else Some(this.text.charAt(this.position))
    }
   
    private def advance(): Unit = {
        this.position += 1
    }

    private def skip_whitespace(): Unit = {
        while (!current_char.isEmpty && current_char.get.isWhitespace) {
            advance()
        }
    }

    private def get_integer(): Int = {
        val begin: Int = position
        while( !current_char.isEmpty && current_char.get.isDigit ) {
            advance()
        }
        return text.substring(begin, position).toInt
    }

    def nextToken(): Token = {
        while( !current_char.isEmpty ) {
            if( current_char.get.isWhitespace ) { 
                skip_whitespace()
            }

            if( current_char.get.isDigit ) {
                return Token.Integer(get_integer())
            }
            if( current_char.get == '+') {
                advance()
                return Token.Plus
            }
        }
        Token.EOF
    }

}

@main def main(): Unit = {
    val text: String = "   42   + 17  "
    val tokenizer = Tokenizer(text)
    println(tokenizer.nextToken())
    println(tokenizer.nextToken())
    println(tokenizer.nextToken())
}