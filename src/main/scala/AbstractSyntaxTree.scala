package muse

import scala.collection.mutable.Map

trait AST {
    def tree(prefix: String = ""): Unit
    def visit(global_state: Map[String, Int]): Unit = {
        println("Default implementation")
    }
}
class BinaryOperatorAST(left: AST, operator: Token, right: AST) extends AST {
    override def tree(prefix: String): Unit = {
        println(s"${prefix}Binary operator (${operator.value})")
        left.tree(prefix + " | ")
        right.tree(prefix + " | ")
    }
}

class IntegerAST(token: Token) extends AST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Integer: " + token.value)
    }
}

class UnaryOperatorAST(operator: Token, expresion: AST) extends AST {
    override def tree(prefix: String): Unit = {
        println(s"${prefix}Unary operator (${operator.value})")
        expresion.tree(prefix + " | ")
    }    
}

class VariableAST(token: Token) extends AST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Variable: " + token.value)
    }
}

trait StatementAST extends AST

class CompoundAST(statement_list: List[AST]) extends StatementAST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Compound statement")
        for (child <- statement_list) {
            child.tree(prefix + " | ")
        }
    }

    override def visit(global_state: Map[String, Int]): Unit = {
        for (child <- statement_list) {
            child.visit(global_state)
        }
    }
}
class AssignAST(left: AST, operator: Token, right: AST) extends StatementAST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Assign statement")
        left.tree(prefix + " | ")
        right.tree(prefix + " | ")
    }

    override def visit(global_state: Map[String, Int]): Unit = {
        val variable_name: String = left.token.value
        global_state(variable_name) = right.token.value.toInt
    }
}

class EmptyStatementAST extends StatementAST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Empty statement")
    }
}
