package muse

class Parser(lexer: Lexer) {
    var current_token: Token = lexer.next_token()
    
    def eat(tpe: Token.Type): Unit = {
        if (current_token.tpe == tpe) {
            current_token = lexer.next_token()
        } else {
            println(s"Unexpected token: $tpe")
        }
    }

    def program(): StatementAST = {
        val node: CompoundAST = compound_statement()
        return node
    }

    def compound_statement(): CompoundAST = {
        eat(Token.LeftBrace)
        val nodes: List[StatementAST] = statement_list()
        eat(Token.RightBrace)
        return CompoundAST(nodes)
    }

    def statement_list(): List[StatementAST] = {
        import scala.collection.mutable.ListBuffer
        
        var current: StatementAST = statement()
        var nodes = ListBuffer[StatementAST](current)
        while (!current.isInstanceOf[EmptyStatementAST]) {
            current = statement()
            nodes += current
        }
        return nodes.toList
    }

    def statement(): StatementAST = {
        if (current_token.tpe == Token.LeftBrace) {
            return compound_statement()
        } else if (current_token.tpe == Token.Id) {
            return assignmen_statement()
        } else {
            return empty_statement()
        }
    }

    def assignmen_statement(): StatementAST = {
        val left: VariableAST = variable()
        val operator: Token = current_token
        eat(Token.Assign)
        val right: EvaluatableAST = expresion()
        return AssignAST(left, operator, right)
    }

    def variable(): VariableAST = {
        val node: VariableAST = VariableAST(current_token)
        eat(Token.Id)
        return node
    }

    def empty_statement(): EmptyStatementAST = {
        return EmptyStatementAST()
    }

    def expresion(): EvaluatableAST = {
        var node: EvaluatableAST = term()
        while (current_token.tpe == Token.Plus || current_token.tpe == Token.Minus) {
            val operator: Token = current_token
            eat(current_token.tpe)
            node = BinaryOperatorAST(node, operator, term())
        }
        return node
    }

    def term(): EvaluatableAST = {
        var node: EvaluatableAST = factor()
        while(current_token.tpe == Token.Multiply || current_token.tpe == Token.Divide) {
            val operator: Token = current_token
            eat(current_token.tpe)
            node = BinaryOperatorAST(node, operator, factor())
        }
        return node
    }

    def factor(): EvaluatableAST = {
        val token: Token = current_token
        if (token.tpe == Token.Plus || token.tpe == Token.Minus) {
            eat(token.tpe)
            return UnaryOperatorAST(token, factor())
        }
        if (token.tpe == Token.Integer) {
            eat(token.tpe)
            return IntegerAST(token)
        }
        if(token.tpe == Token.LeftParenthesis) {
            eat(Token.LeftParenthesis)
            val node: EvaluatableAST = expresion()
            eat(Token.RightParenthesis)
            return node
        }
        return variable()

    }

    def parse(): StatementAST = {
        val root: StatementAST = program()
        if (current_token.tpe != Token.EOF) {
            println("Something went wrong") // TODO: Error handling
        }
        return root
    }
}