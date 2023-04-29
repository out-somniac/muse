package muse

def main(args: Array[String]): Unit = {
    import scala.io.Source._
    val code: String = fromFile("tests/programs/main.mu").getLines.mkString
    val lex = Lexer(code)
    lex.next_token();
}