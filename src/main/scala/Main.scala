package muse


@main def main(): Unit = {
    import Token.Type
    val input: String = "7 + 3 * (10 / (12 / (3 + 1) - 1)) / (2 + 3) - 5 - 3 + (8)"
    val tokens: List[Token[Any]] = Lexer(input).lex()
    val result = Interpreter(tokens).expression()
    println(result)
}