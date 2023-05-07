package muse

def main(args: Array[String]): Unit = {
    import scala.io.Source._
    import scala.collection.mutable.Map

    val code: String = fromFile("tests/programs/main.mu").mkString
    val lexer = Lexer(code)
    var token: Token = lexer.next_token()
    val parser = Parser(lexer)
    val root: StatementAST = parser.parse()
    val interpreter: Interpreter = Interpreter(root)
    val global_scope: Map[String, Int] = interpreter.interpret()
    println("Variable values are")
    for ((key, value) <- global_scope) {
        println(s"${key} -> ${value}")
    }
}