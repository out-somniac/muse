package muse

def main(args: Array[String]): Unit = {
    import scala.io.Source._
    val code: String = fromFile("tests/programs/main.mu").getLines.mkString
    val lex = Lexer(code)
    var token: Token = lex.next_token()
    while (token.tpe != Token.EOF) {
        println(token)
        token = lex.next_token()
    }
}