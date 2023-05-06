package muse

import scala.collection.mutable.Map

class Interpreter(val root: StatementAST) {
    private var global_state = Map[String, Int]()
    
    def show_tree(): Unit = {
        root.tree()
    }

    def interpret(): Map[String, Int] =  {
        root.visit(global_state)
        return global_state
    }
}