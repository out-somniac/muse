package muse

class Parser(lexer: Lexer) {
    var current_token: Token = lexer.next_token()
    
    def eat(token: Token): Unit = {
        if (current_token.tpe == token.tpe) {
            current_token = lexer.next_token()
        } else {
            println(s"Unexpected token: $token")
        }
    }

    def program(): AST = {
        val node: AST = compound_statement()
        return node
    }

    def compound_statement(): AST = {
        eat(Token(Token.LeftBrace, "{"))
        val nodes: List[AST] = statement_list()
        eat(Token(Token.RightBrace, "}"))
        return CompoundAST(nodes)
    }

    def statement_list(): List[AST] = {
        import scala.collection.mutable.ListBuffer
        
        var current: AST = statement()
        var nodes = ListBuffer[AST](current)
        while (!current.isInstanceOf[EmptyStatementAST]) {
            current = statement()
            nodes += current
        }
        return nodes.toList
    }

    def statement(): AST = {
        if (current_token.tpe == Token.LeftBrace) {
            return compound_statement()
        } else if (current_token.tpe == Token.Id) {
            return assignmen_statement()
        } else {
            return empty_statement()
        }
    }

    def assignmen_statement(): AST = {
        val left: AST = variable()
        val operator: Token = current_token
        eat(Token(Token.Assign, ":="))
        val right: AST = expresion()
        return AssignAST(left, operator, right)
    }

    def variable(): AST = {
        val node: AST = VariableAST(current_token)
        eat(Token(Token.Id, ""))
        return node
    }

    def empty_statement(): AST = {
        return EmptyStatementAST()
    }
}