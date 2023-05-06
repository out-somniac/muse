package muse

def main(args: Array[String]): Unit = {
    import scala.io.Source._
    val code: String = fromFile("tests/programs/main.mu").getLines.mkString
    val lexer = Lexer(code)
    val parser = Parser(lexer)
    val root: AST = parser.parse()
    val interpreter: Interpreter = Interpreter(root)
    
}