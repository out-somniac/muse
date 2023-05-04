package muse

trait AST
class BinaryOperator(left: AST, operator: Token, right: AST) extends AST
class IntegerAST(token: Token) extends AST
class UnaryOperatorAST(operator: Token, expresion: AST) extends AST
class CompoundAST(statement_list: List[AST]) extends AST
class AssignAST(left: AST, operator: Token, right: AST) extends AST
class VariableAST(token: Token) extends AST
class EmptyStatementAST extends AST