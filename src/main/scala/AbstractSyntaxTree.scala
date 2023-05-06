package muse

import scala.collection.mutable.Map

trait AST {
    def tree(prefix: String = ""): Unit
}

// Statements - Don't evaluate to anything

trait StatementAST extends AST {
    def visit(global_state: Map[String, Int]): Unit = {
        println("Default implementation")
    }
}

class CompoundAST(statement_list: List[StatementAST]) extends StatementAST {
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
class AssignAST(left: VariableAST, operator: Token, right: EvaluatableAST) extends StatementAST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Assign statement")
        left.tree(prefix + " | ")
        right.tree(prefix + " | ")
    }

    override def visit(global_state: Map[String, Int]): Unit = {
        val variable_name: String = left.name
        global_state(variable_name) = right.visit(global_state)
    }
}

class EmptyStatementAST extends StatementAST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Empty statement")
    }

    override def visit(global_state: Map[String, Int]): Unit = {}
}

// Subtrees that evaluate to a numerical value

trait EvaluatableAST extends AST {
    def visit(global_state: Map[String, Int]): Int = {
        println("Default implementatio")
        return -1
    }
}

class BinaryOperatorAST(left: EvaluatableAST, operator: Token, right: EvaluatableAST) extends EvaluatableAST {
    override def tree(prefix: String): Unit = {
        println(s"${prefix}Binary operator (${operator.value})")
        left.tree(prefix + " | ")
        right.tree(prefix + " | ")
    }

    override def visit(global_state: Map[String, Int]): Int = {
        val left_value: Int = left.visit(global_state)
        val right_value: Int = right.visit(global_state)
        operator.value match {
            case "+" => return left_value + right_value
            case "-" => return left_value - right_value
            case "*" => return left_value * right_value
            case "/" => return left_value / right_value
        }
    }
}

class IntegerAST(token: Token) extends EvaluatableAST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Integer: " + token.value)
    }

    override def visit(global_state: Map[String, Int]): Int = {
        return token.value.toInt
    }
}

class UnaryOperatorAST(operator: Token, expresion: EvaluatableAST) extends EvaluatableAST {
    override def tree(prefix: String): Unit = {
        println(s"${prefix}Unary operator (${operator.value})")
        expresion.tree(prefix + " | ")
    }
    
    override def visit(global_state: Map[String, Int]): Int = {
        val value: Int = expresion.visit(global_state)
        operator.value match {
            case "+" => return value
            case "-" => return -value
        }
    }
}

class VariableAST(token: Token) extends EvaluatableAST {
    override def tree(prefix: String): Unit = {
        println(prefix + "Variable: " + token.value)
    }

    override def visit(global_state: Map[String, Int]): Int = {
        return global_state(token.value)
    }

    def name: String = token.value
}

